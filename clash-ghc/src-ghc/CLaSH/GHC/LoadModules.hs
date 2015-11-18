{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CLaSH.GHC.LoadModules
  ( loadModules
  , ghcLibDir
  )
where

-- External Modules
import           Data.List                    (nub)
import           Data.Maybe                   (listToMaybe)
import           CLaSH.Annotations.TopEntity  (TopEntity)
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hGetLine)
import           System.Process               (runInteractiveCommand,
                                               waitForProcess)

-- GHC API
import qualified Annotations
import qualified CoreSyn
import           DynFlags                     (GeneralFlag (..))
import qualified DynFlags
import qualified GHC
import qualified HscMain
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import qualified Serialized
import qualified TidyPgm

import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM
import qualified Var
import qualified FamInst
import qualified FamInstEnv

-- Internal Modules
import           CLaSH.GHC.LoadInterfaceFiles
import           CLaSH.Util                   (curLoc,first)

ghcLibDir :: IO FilePath
ghcLibDir = do (libDir,exitCode) <- getProcessOutput $ "ghc-" ++ TOOL_VERSION_ghc ++ " --print-libdir"
               case exitCode of
                  ExitSuccess   -> return libDir
                  ExitFailure i -> error $ "Calling GHC failed with: " ++ show i

getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command =
     -- Create the process
  do (_, pOut, _, handle) <- runInteractiveCommand command
     -- Wait for the process to finish and store its exit code
     exitCode <- waitForProcess handle
     -- Get the standard output.
     output   <- hGetLine pOut
     -- return both the output and the exit code.
     return (output, exitCode)

loadModules ::
  String
  -> Maybe (DynFlags.DynFlags)
  -> IO ( [(CoreSyn.CoreBndr, CoreSyn.CoreExpr)]   -- Binders
        , [(CoreSyn.CoreBndr,Int)]                 -- Class operations
        , [CoreSyn.CoreBndr]                       -- Unlocatable Expressions
        , FamInstEnv.FamInstEnvs
        , Maybe TopEntity
        )
loadModules modName dflagsM = GHC.defaultErrorHandler DynFlags.defaultFatalMessager
                              DynFlags.defaultFlushOut $ do
  libDir <- MonadUtils.liftIO ghcLibDir

  GHC.runGhc (Just libDir) $ do
    dflags <- case dflagsM of
                Just df -> return df
                Nothing -> do
                  df <- GHC.getSessionDynFlags
                  let dfEn = foldl DynFlags.xopt_set df
                                [ DynFlags.Opt_TemplateHaskell
                                , DynFlags.Opt_DataKinds
                                , DynFlags.Opt_TypeOperators
                                , DynFlags.Opt_FlexibleContexts
                                , DynFlags.Opt_ConstraintKinds
                                , DynFlags.Opt_TypeFamilies
                                , DynFlags.Opt_BinaryLiterals
                                , DynFlags.Opt_ExplicitNamespaces
                                , DynFlags.Opt_KindSignatures
                                ]
                  let dfDis = foldl DynFlags.xopt_unset dfEn
                                [ DynFlags.Opt_ImplicitPrelude
                                , DynFlags.Opt_MonomorphismRestriction
                                ]
                  let ghcTyLitNormPlugin = GHC.mkModuleName "GHC.TypeLits.Normalise"
                  let dfPlug = dfDis { DynFlags.pluginModNames = nub $
                                          ghcTyLitNormPlugin : DynFlags.pluginModNames dfDis
                                     }
                  return dfPlug

    let dflags1 = dflags
#if __GLASGOW_HASKELL__ >= 711
                    { DynFlags.reductionDepth = 1000
#else
                    { DynFlags.ctxtStkDepth = 1000
#endif
                    , DynFlags.optLevel = 2
                    , DynFlags.ghcMode  = GHC.CompManager
                    , DynFlags.ghcLink  = GHC.LinkInMemory
                    , DynFlags.hscTarget = DynFlags.defaultObjectTarget
                                             (DynFlags.targetPlatform dflags)
                    }
    let dflags2 = wantedOptimizationFlags dflags1
    let ghcDynamic = case lookup "GHC Dynamic" (DynFlags.compilerInfo dflags) of
                      Just "YES" -> True
                      _          -> False
    let dflags3 = if ghcDynamic then DynFlags.gopt_set dflags2 DynFlags.Opt_BuildDynamicToo
                                else dflags2
    _ <- GHC.setSessionDynFlags dflags3
    target <- GHC.guessTarget modName Nothing
    GHC.setTargets [target]
    ldRes <- GHC.load GHC.LoadAllTargets
    case ldRes of
      GHC.Succeeded -> do
        modGraph <- GHC.getModuleGraph
        let modGraph' = map disableOptimizationsFlags modGraph
        tidiedMods <- mapM (\m -> do { pMod  <- parseModule m
                                     ; tcMod <- GHC.typecheckModule pMod
                                     ; dsMod <- fmap GHC.coreModule $ GHC.desugarModule tcMod
                                     ; hsc_env <- GHC.getSession
                                     ; simpl_guts <- MonadUtils.liftIO $ HscMain.hscSimplify hsc_env dsMod
                                     ; (tidy_guts,_) <- MonadUtils.liftIO $ TidyPgm.tidyProgram hsc_env simpl_guts
                                     ; let pgm        = HscTypes.cg_binds tidy_guts
                                     ; let modFamInstEnv = TcRnTypes.tcg_fam_inst_env $ fst $ GHC.tm_internals_ tcMod
                                     ; return (CoreSyn.flattenBinds pgm,modFamInstEnv)
                                     }
                             ) modGraph'

        let (binders,modFamInstEnvs) = first concat $ unzip tidiedMods
            modFamInstEnvs'          = foldr UniqFM.plusUFM UniqFM.emptyUFM modFamInstEnvs

        (externalBndrs,clsOps,unlocatable) <- loadExternalExprs
                                                (map snd binders)
                                                (map fst binders)

        hscEnv <- GHC.getSession
        famInstEnvs <- TcRnMonad.liftIO $ TcRnMonad.initTcForLookup hscEnv FamInst.tcGetFamInstEnvs
        topEntityAnnotations <- findCLaSHAnnotations (map fst binders)
        let topEntM = listToMaybe topEntityAnnotations

        return (binders ++ externalBndrs,clsOps,unlocatable,(fst famInstEnvs,modFamInstEnvs'),topEntM)
      GHC.Failed -> Panic.pgmError $ $(curLoc) ++ "failed to load module: " ++ modName

findCLaSHAnnotations :: GHC.GhcMonad m
                     => [CoreSyn.CoreBndr]
                     -> m [TopEntity]
findCLaSHAnnotations bndrs = do
  let deserializer = Serialized.deserializeWithData
      targets      = map (Annotations.NamedTarget . Var.varName) bndrs
  concat <$> mapM (GHC.findGlobalAnns deserializer) targets

parseModule :: GHC.GhcMonad m => GHC.ModSummary -> m GHC.ParsedModule
parseModule modSum = do
  (GHC.ParsedModule pmModSum pmParsedSource extraSrc anns) <-
    GHC.parseModule modSum
  return (GHC.ParsedModule
            (disableOptimizationsFlags pmModSum)
            pmParsedSource extraSrc anns)

disableOptimizationsFlags :: GHC.ModSummary -> GHC.ModSummary
disableOptimizationsFlags ms@(GHC.ModSummary {..})
  = ms {GHC.ms_hspp_opts = dflags}
  where
    dflags = wantedOptimizationFlags (ms_hspp_opts
              { DynFlags.optLevel = 2
#if __GLASGOW_HASKELL__ >= 711
              , DynFlags.reductionDepth = 1000
#else
              , DynFlags.ctxtStkDepth = 1000
#endif
              })

wantedOptimizationFlags :: GHC.DynFlags -> GHC.DynFlags
wantedOptimizationFlags df = foldl DynFlags.gopt_unset (foldl DynFlags.gopt_set df wanted) unwanted
  where
    wanted = [ Opt_CSE -- CSE
             , Opt_Specialise -- Specialise on types, specialise type-class-overloaded function defined in this module for the types
             , Opt_DoLambdaEtaExpansion -- transform nested series of lambdas into one with multiple arguments, helps us achieve only top-level lambdas
             , Opt_CaseMerge -- We want fewer case-statements
             , Opt_DictsCheap -- Makes dictionaries seem cheap to optimizer: hopefully inline
             , Opt_SimpleListLiterals -- Avoids 'build' rule
             , Opt_ExposeAllUnfoldings -- We need all the unfoldings we can get
             , Opt_ForceRecomp -- Force recompilation: never bad
             , Opt_EnableRewriteRules -- Reduce number of functions
             , Opt_SimplPreInlining -- Inlines simple functions, we only care about the major first-order structure
             , Opt_StaticArgumentTransformation -- Turn on the static argument transformation, which turns a recursive function into a non-recursive one with a local recursive loop.
             , Opt_FloatIn -- Moves let-bindings inwards, although it defeats the normal-form with a single top-level let-binding, it helps with other transformations
             , Opt_DictsStrict -- Hopefully helps remove class method selectors
             , Opt_DmdTxDictSel -- I think demand and strictness are related, strictness helps with dead-code, enable
             , Opt_Strictness -- Strictness analysis helps with dead-code analysis. However, see [NOTE: CPR breaks CLaSH]
             ]

    unwanted = [ Opt_LiberateCase -- Perform unrolling of recursive RHS: avoid
               , Opt_SpecConstr -- Creates local-functions: avoid
               , Opt_IgnoreAsserts -- We don't care about assertions
               , Opt_DoEtaReduction -- We want eta-expansion
               , Opt_UnboxStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_UnboxSmallStrictFields -- Unboxed types are not handled properly: avoid
               , Opt_Vectorise -- Don't care
               , Opt_VectorisationAvoidance -- Don't care
               , Opt_RegsGraph -- Don't care
               , Opt_RegsGraph -- Don't care
               , Opt_PedanticBottoms -- Stops eta-expansion through case: avoid
               , Opt_LlvmTBAA -- Don't care
               , Opt_CmmSink -- Don't care
               , Opt_CmmElimCommonBlocks -- Don't care
               , Opt_OmitYields -- Don't care
               , Opt_IgnoreInterfacePragmas -- We need all the unfoldings we can get
               , Opt_OmitInterfacePragmas -- We need all the unfoldings we can get
               , Opt_IrrefutableTuples -- Introduce irrefutPatError: avoid
               , Opt_Loopification -- STG pass, don't care
#if __GLASGOW_HASKELL__ >= 712
               , Opt_CprAnal -- The worker/wrapper introduced by CPR breaks CLaSH, see [NOTE: CPR breaks CLaSH]
#endif
               ]

-- [NOTE: CPR breaks CLaSH]
-- We used to completely disable strictness analysis because it causes GHC to
-- do the so-called "Constructed Product Result" (CPR) analysis, which in turn
-- creates an annoying worker/wrapper which does the following:
--
--   * Scrutinise a Signal, and pack the head and tail of the
--     Signal in an unboxed tuple.
--   * Scrutinise on the unboxed tuple, and recreate the Signal.
--
-- This is problematic because the 'Signal' type is essentially treated as a "transparent"
-- type by the CLaSH compiler, so observing its constructor leads to all kinds
-- of problems.
--
-- The current solution is to disable strictness analysis in "CLaSH.Signal.Internal"
-- so that functions manipulating 'Signal' constructor do not get a strictness/
-- demand/CPR annotation, which in turn ensures GHC doesn't create worker/wrappers
-- for when these functions are called in user code.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, CLaSH cannot deal with this recursive type and the
-- recursive functions involved, and hence we need to disable this useful transformation. After
-- everything is done properly, we should enable it again.
