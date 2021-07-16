module Monadoc.Utility.Ghc where

import qualified GHC.Data.Bag
import qualified GHC.Data.EnumSet
import qualified GHC.Data.FastString
import qualified GHC.Data.StringBuffer
import qualified GHC.Driver.Session
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type
import qualified GHC.Parser
import qualified GHC.Parser.Lexer
import qualified GHC.Settings
import qualified GHC.Types.SrcLoc
import qualified GHC.Utils.Error
import qualified GHC.Utils.Misc
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr.Colour

newtype Errors = Errors
    { unwrapErrors :: GHC.Utils.Error.ErrorMessages
    }

instance Show Errors where
    show = show . GHC.Data.Bag.bagToList . unwrapErrors

newtype Module = Module
    { unwrapModule :: GHC.Types.SrcLoc.Located GHC.Hs.HsModule
    }

instance Show Module where
    show = GHC.Utils.Outputable.showSDocUnsafe . GHC.Utils.Outputable.ppr . unwrapModule

parseModule
    :: FilePath
    -> String
    -> Either Errors Module
parseModule filePath string =
    let
        ghcNameVersion = GHC.Driver.Session.GhcNameVersion
            { GHC.Driver.Session.ghcNameVersion_programName = error "ghcNameVersion_programName"
            , GHC.Driver.Session.ghcNameVersion_projectVersion = error "ghcNameVersion_projectVersion"
            }
        fileSettings = GHC.Driver.Session.FileSettings
            { GHC.Driver.Session.fileSettings_ghciUsagePath = error "fileSettings_ghciUsagePath"
            , GHC.Driver.Session.fileSettings_ghcUsagePath = error "fileSettings_ghcUsagePath"
            , GHC.Driver.Session.fileSettings_globalPackageDatabase = error "fileSettings_globalPackageDatabase"
            , GHC.Driver.Session.fileSettings_tmpDir = error "fileSettings_tmpDir"
            , GHC.Driver.Session.fileSettings_toolDir = error "fileSettings_toolDir"
            , GHC.Driver.Session.fileSettings_topDir = error "fileSettings_topDir"
            }
        toolSettings = GHC.Settings.ToolSettings
            { GHC.Settings.toolSettings_ccSupportsNoPie = error "toolSettings_ccSupportsNoPie"
            , GHC.Settings.toolSettings_extraGccViaCFlags = error "toolSettings_extraGccViaCFlags"
            , GHC.Settings.toolSettings_ldIsGnuLd = error "toolSettings_ldIsGnuLd"
            , GHC.Settings.toolSettings_ldSupportsBuildId = error "toolSettings_ldSupportsBuildId"
            , GHC.Settings.toolSettings_ldSupportsCompactUnwind = error "toolSettings_ldSupportsCompactUnwind"
            , GHC.Settings.toolSettings_ldSupportsFilelist = error "toolSettings_ldSupportsFilelist"
            , GHC.Settings.toolSettings_opt_a = error "toolSettings_opt_a"
            , GHC.Settings.toolSettings_opt_c = error "toolSettings_opt_c"
            , GHC.Settings.toolSettings_opt_cxx = error "toolSettings_opt_cxx"
            , GHC.Settings.toolSettings_opt_F = error "toolSettings_opt_F"
            , GHC.Settings.toolSettings_opt_i = error "toolSettings_opt_i"
            , GHC.Settings.toolSettings_opt_l = error "toolSettings_opt_l"
            , GHC.Settings.toolSettings_opt_L = error "toolSettings_opt_L"
            , GHC.Settings.toolSettings_opt_lc = error "toolSettings_opt_lc"
            , GHC.Settings.toolSettings_opt_lcc = error "toolSettings_opt_lcc"
            , GHC.Settings.toolSettings_opt_lm = error "toolSettings_opt_lm"
            , GHC.Settings.toolSettings_opt_lo = error "toolSettings_opt_lo"
            , GHC.Settings.toolSettings_opt_P = error "toolSettings_opt_P"
            , GHC.Settings.toolSettings_opt_P_fingerprint = error "toolSettings_opt_P_fingerprint"
            , GHC.Settings.toolSettings_opt_windres = error "toolSettings_opt_windres"
            , GHC.Settings.toolSettings_pgm_a = error "toolSettings_pgm_a"
            , GHC.Settings.toolSettings_pgm_ar = error "toolSettings_pgm_ar"
            , GHC.Settings.toolSettings_pgm_c = error "toolSettings_pgm_c"
            , GHC.Settings.toolSettings_pgm_dll = error "toolSettings_pgm_dll"
            , GHC.Settings.toolSettings_pgm_F = error "toolSettings_pgm_F"
            , GHC.Settings.toolSettings_pgm_i = error "toolSettings_pgm_i"
            , GHC.Settings.toolSettings_pgm_install_name_tool = error "toolSettings_pgm_install_name_tool"
            , GHC.Settings.toolSettings_pgm_l = error "toolSettings_pgm_l"
            , GHC.Settings.toolSettings_pgm_L = error "toolSettings_pgm_L"
            , GHC.Settings.toolSettings_pgm_lc = error "toolSettings_pgm_lc"
            , GHC.Settings.toolSettings_pgm_lcc = error "toolSettings_pgm_lcc"
            , GHC.Settings.toolSettings_pgm_libtool = error "toolSettings_pgm_libtool"
            , GHC.Settings.toolSettings_pgm_lm = error "toolSettings_pgm_lm"
            , GHC.Settings.toolSettings_pgm_lo = error "toolSettings_pgm_lo"
            , GHC.Settings.toolSettings_pgm_otool = error "toolSettings_pgm_otool"
            , GHC.Settings.toolSettings_pgm_P = error "toolSettings_pgm_P"
            , GHC.Settings.toolSettings_pgm_ranlib = error "toolSettings_pgm_ranlib"
            , GHC.Settings.toolSettings_pgm_T = error "toolSettings_pgm_T"
            , GHC.Settings.toolSettings_pgm_windres = error "toolSettings_pgm_windres"
            }
        platformMisc = GHC.Driver.Session.PlatformMisc
            { GHC.Driver.Session.platformMisc_ghcDebugged = error "platformMisc_ghcDebugged"
            , GHC.Driver.Session.platformMisc_ghcRTSWays = error "platformMisc_ghcRTSWays"
            , GHC.Driver.Session.platformMisc_ghcRtsWithLibdw = error "platformMisc_ghcRtsWithLibdw"
            , GHC.Driver.Session.platformMisc_ghcThreaded = error "platformMisc_ghcThreaded"
            , GHC.Driver.Session.platformMisc_ghcWithInterpreter = error "platformMisc_ghcWithInterpreter"
            , GHC.Driver.Session.platformMisc_ghcWithSMP = error "platformMisc_ghcWithSMP"
            , GHC.Driver.Session.platformMisc_libFFI = error "platformMisc_libFFI"
            , GHC.Driver.Session.platformMisc_llvmTarget = error "platformMisc_llvmTarget"
            , GHC.Driver.Session.platformMisc_targetPlatformString = error "platformMisc_targetPlatformString"
            }
        safeHaskell = GHC.Driver.Session.Sf_Ignore
        extensionFlags = GHC.Data.EnumSet.fromList
            [ GHC.LanguageExtensions.Type.NumDecimals
            ]
        generalFlags = GHC.Data.EnumSet.fromList
            [ GHC.Driver.Session.Opt_ForceRecomp
            ]
        dumpFlags = GHC.Data.EnumSet.fromList
            [ GHC.Driver.Session.Opt_D_dump_json
            ]
        verbosity = 0 :: Int
        useColor = GHC.Utils.Misc.Never
        colScheme = GHC.Utils.Ppr.Colour.defaultScheme
        pprUserLength = 0 :: Int
        pprCols = 0 :: Int
        useUnicode = False
        dynFlags = GHC.Driver.Session.DynFlags
            { GHC.Driver.Session.avx = error "avx"
            , GHC.Driver.Session.avx2 = error "avx2"
            , GHC.Driver.Session.avx512cd = error "avx512cd"
            , GHC.Driver.Session.avx512er = error "avx512er"
            , GHC.Driver.Session.avx512f = error "avx512f"
            , GHC.Driver.Session.avx512pf = error "avx512pf"
            , GHC.Driver.Session.binBlobThreshold = error "binBlobThreshold"
            , GHC.Driver.Session.bmiVersion = error "bmiVersion"
            , GHC.Driver.Session.cachedPlugins = error "cachedPlugins"
            , GHC.Driver.Session.canGenerateDynamicToo = error "canGenerateDynamicToo"
            , GHC.Driver.Session.canUseColor = error "canUseColor"
            , GHC.Driver.Session.cfgWeightInfo = error "cfgWeightInfo"
            , GHC.Driver.Session.cmdlineFrameworks = error "cmdlineFrameworks"
            , GHC.Driver.Session.cmmProcAlignment = error "cmmProcAlignment"
            , GHC.Driver.Session.colScheme = colScheme
            , GHC.Driver.Session.debugLevel = error "debugLevel"
            , GHC.Driver.Session.depExcludeMods = error "depExcludeMods"
            , GHC.Driver.Session.depIncludeCppDeps = error "depIncludeCppDeps"
            , GHC.Driver.Session.depIncludePkgDeps = error "depIncludePkgDeps"
            , GHC.Driver.Session.depMakefile = error "depMakefile"
            , GHC.Driver.Session.depSuffixes = error "depSuffixes"
            , GHC.Driver.Session.dirsToClean = error "dirsToClean"
            , GHC.Driver.Session.dump_action = error "dump_action"
            , GHC.Driver.Session.dumpDir = error "dumpDir"
            , GHC.Driver.Session.dumpFlags = dumpFlags
            , GHC.Driver.Session.dumpPrefix = error "dumpPrefix"
            , GHC.Driver.Session.dumpPrefixForce = error "dumpPrefixForce"
            , GHC.Driver.Session.dylibInstallName = error "dylibInstallName"
            , GHC.Driver.Session.dynHiSuf = error "dynHiSuf"
            , GHC.Driver.Session.dynLibLoader = error "dynLibLoader"
            , GHC.Driver.Session.dynObjectSuf = error "dynObjectSuf"
            , GHC.Driver.Session.dynOutputFile = error "dynOutputFile"
            , GHC.Driver.Session.enableTimeStats = error "enableTimeStats"
            , GHC.Driver.Session.extensionFlags = extensionFlags
            , GHC.Driver.Session.extensions = error "extensions"
            , GHC.Driver.Session.fatalWarningFlags = error "fatalWarningFlags"
            , GHC.Driver.Session.fileSettings = fileSettings
            , GHC.Driver.Session.filesToClean = error "filesToClean"
            , GHC.Driver.Session.floatLamArgs = error "floatLamArgs"
            , GHC.Driver.Session.flushErr = error "flushErr"
            , GHC.Driver.Session.flushOut = error "flushOut"
            , GHC.Driver.Session.frameworkPaths = error "frameworkPaths"
            , GHC.Driver.Session.frontendPluginOpts = error "frontendPluginOpts"
            , GHC.Driver.Session.generalFlags = generalFlags
            , GHC.Driver.Session.generatedDumps = error "generatedDumps"
            , GHC.Driver.Session.ghcHeapSize = error "ghcHeapSize"
            , GHC.Driver.Session.ghciHistSize = error "ghciHistSize"
            , GHC.Driver.Session.ghciScripts = error "ghciScripts"
            , GHC.Driver.Session.ghcLink = error "ghcLink"
            , GHC.Driver.Session.ghcMode = error "ghcMode"
            , GHC.Driver.Session.ghcNameVersion = ghcNameVersion
            , GHC.Driver.Session.ghcVersionFile = error "ghcVersionFile"
            , GHC.Driver.Session.haddockOptions = error "haddockOptions"
            , GHC.Driver.Session.hcSuf = error "hcSuf"
            , GHC.Driver.Session.hiDir = error "hiDir"
            , GHC.Driver.Session.hieDir = error "hieDir"
            , GHC.Driver.Session.hieSuf = error "hieSuf"
            , GHC.Driver.Session.historySize = error "historySize"
            , GHC.Driver.Session.hiSuf = error "hiSuf"
            , GHC.Driver.Session.homeUnitId = error "homeUnitId"
            , GHC.Driver.Session.homeUnitInstanceOfId = error "homeUnitInstanceOfId"
            , GHC.Driver.Session.homeUnitInstantiations = error "homeUnitInstantiations"
            , GHC.Driver.Session.hooks = error "hooks"
            , GHC.Driver.Session.hpcDir = error "hpcDir"
            , GHC.Driver.Session.hscTarget = error "hscTarget"
            , GHC.Driver.Session.ignorePackageFlags = error "ignorePackageFlags"
            , GHC.Driver.Session.importPaths = error "importPaths"
            , GHC.Driver.Session.includePaths = error "includePaths"
            , GHC.Driver.Session.incoherentOnLoc = error "incoherentOnLoc"
            , GHC.Driver.Session.initialUnique = error "initialUnique"
            , GHC.Driver.Session.inlineCheck = error "inlineCheck"
            , GHC.Driver.Session.interactivePrint = error "interactivePrint"
            , GHC.Driver.Session.language = error "language"
            , GHC.Driver.Session.ldInputs = error "ldInputs"
            , GHC.Driver.Session.liberateCaseThreshold = error "liberateCaseThreshold"
            , GHC.Driver.Session.libraryPaths = error "libraryPaths"
            , GHC.Driver.Session.liftLamsKnown = error "liftLamsKnown"
            , GHC.Driver.Session.liftLamsNonRecArgs = error "liftLamsNonRecArgs"
            , GHC.Driver.Session.liftLamsRecArgs = error "liftLamsRecArgs"
            , GHC.Driver.Session.llvmConfig = error "llvmConfig"
            , GHC.Driver.Session.log_action = error "log_action"
            , GHC.Driver.Session.mainFunIs = error "mainFunIs"
            , GHC.Driver.Session.mainModIs = error "mainModIs"
            , GHC.Driver.Session.maxErrors = error "maxErrors"
            , GHC.Driver.Session.maxInlineAllocSize = error "maxInlineAllocSize"
            , GHC.Driver.Session.maxInlineMemcpyInsns = error "maxInlineMemcpyInsns"
            , GHC.Driver.Session.maxInlineMemsetInsns = error "maxInlineMemsetInsns"
            , GHC.Driver.Session.maxPmCheckModels = error "maxPmCheckModels"
            , GHC.Driver.Session.maxRefHoleFits = error "maxRefHoleFits"
            , GHC.Driver.Session.maxRelevantBinds = error "maxRelevantBinds"
            , GHC.Driver.Session.maxSimplIterations = error "maxSimplIterations"
            , GHC.Driver.Session.maxUncoveredPatterns = error "maxUncoveredPatterns"
            , GHC.Driver.Session.maxValidHoleFits = error "maxValidHoleFits"
            , GHC.Driver.Session.maxWorkerArgs = error "maxWorkerArgs"
            , GHC.Driver.Session.newDerivOnLoc = error "newDerivOnLoc"
            , GHC.Driver.Session.nextTempSuffix = error "nextTempSuffix"
            , GHC.Driver.Session.nextWrapperNum = error "nextWrapperNum"
            , GHC.Driver.Session.objectDir = error "objectDir"
            , GHC.Driver.Session.objectSuf = error "objectSuf"
            , GHC.Driver.Session.optLevel = error "optLevel"
            , GHC.Driver.Session.outputFile = error "outputFile"
            , GHC.Driver.Session.outputHi = error "outputHi"
            , GHC.Driver.Session.overlapInstLoc = error "overlapInstLoc"
            , GHC.Driver.Session.packageDBFlags = error "packageDBFlags"
            , GHC.Driver.Session.packageEnv = error "packageEnv"
            , GHC.Driver.Session.packageFlags = error "packageFlags"
            , GHC.Driver.Session.parMakeCount = error "parMakeCount"
            , GHC.Driver.Session.pkgTrustOnLoc = error "pkgTrustOnLoc"
            , GHC.Driver.Session.platformConstants = error "platformConstants"
            , GHC.Driver.Session.platformMisc = platformMisc
            , GHC.Driver.Session.pluginModNameOpts = error "pluginModNameOpts"
            , GHC.Driver.Session.pluginModNames = error "pluginModNames"
            , GHC.Driver.Session.pluginPackageFlags = error "pluginPackageFlags"
            , GHC.Driver.Session.pprCols = pprCols
            , GHC.Driver.Session.pprUserLength = pprUserLength
            , GHC.Driver.Session.profAuto = error "profAuto"
            , GHC.Driver.Session.rawSettings = error "rawSettings"
            , GHC.Driver.Session.reductionDepth = error "reductionDepth"
            , GHC.Driver.Session.refLevelHoleFits = error "refLevelHoleFits"
            , GHC.Driver.Session.reverseErrors = error "reverseErrors"
            , GHC.Driver.Session.rtccInfo = error "rtccInfo"
            , GHC.Driver.Session.rtldInfo = error "rtldInfo"
            , GHC.Driver.Session.rtsOpts = error "rtsOpts"
            , GHC.Driver.Session.rtsOptsEnabled = error "rtsOptsEnabled"
            , GHC.Driver.Session.rtsOptsSuggestions = error "rtsOptsSuggestions"
            , GHC.Driver.Session.ruleCheck = error "ruleCheck"
            , GHC.Driver.Session.safeHaskell = safeHaskell
            , GHC.Driver.Session.safeInfer = error "safeInfer"
            , GHC.Driver.Session.safeInferred = error "safeInferred"
            , GHC.Driver.Session.simplPhases = error "simplPhases"
            , GHC.Driver.Session.simplTickFactor = error "simplTickFactor"
            , GHC.Driver.Session.solverIterations = error "solverIterations"
            , GHC.Driver.Session.specConstrCount = error "specConstrCount"
            , GHC.Driver.Session.specConstrRecursive = error "specConstrRecursive"
            , GHC.Driver.Session.specConstrThreshold = error "specConstrThreshold"
            , GHC.Driver.Session.splitInfo = error "splitInfo"
            , GHC.Driver.Session.sseVersion = error "sseVersion"
            , GHC.Driver.Session.staticPlugins = error "staticPlugins"
            , GHC.Driver.Session.strictnessBefore = error "strictnessBefore"
            , GHC.Driver.Session.stubDir = error "stubDir"
            , GHC.Driver.Session.targetPlatform = error "targetPlatform"
            , GHC.Driver.Session.thOnLoc = error "thOnLoc"
            , GHC.Driver.Session.toolSettings = toolSettings
            , GHC.Driver.Session.trace_action = error "trace_action"
            , GHC.Driver.Session.trustFlags = error "trustFlags"
            , GHC.Driver.Session.trustworthyOnLoc = error "trustworthyOnLoc"
            , GHC.Driver.Session.ufCreationThreshold = error "ufCreationThreshold"
            , GHC.Driver.Session.ufDearOp = error "ufDearOp"
            , GHC.Driver.Session.ufDictDiscount = error "ufDictDiscount"
            , GHC.Driver.Session.ufFunAppDiscount = error "ufFunAppDiscount"
            , GHC.Driver.Session.ufUseThreshold = error "ufUseThreshold"
            , GHC.Driver.Session.ufVeryAggressive = error "ufVeryAggressive"
            , GHC.Driver.Session.uniqueIncrement = error "uniqueIncrement"
            , GHC.Driver.Session.unitDatabases = error "unitDatabases"
            , GHC.Driver.Session.unitState = error "unitState"
            , GHC.Driver.Session.useColor = useColor
            , GHC.Driver.Session.useUnicode = useUnicode
            , GHC.Driver.Session.verbosity = verbosity
            , GHC.Driver.Session.warningFlags = error "warningFlags"
            , GHC.Driver.Session.warnSafeOnLoc = error "warnSafeOnLoc"
            , GHC.Driver.Session.warnUnsafeOnLoc = error "warnUnsafeOnLoc"
            , GHC.Driver.Session.ways = error "ways"
            }
        stringBuffer = GHC.Data.StringBuffer.stringToStringBuffer string
        fastString = GHC.Data.FastString.mkFastString filePath
        realSrcLoc = GHC.Types.SrcLoc.mkRealSrcLoc fastString 1 1
        pState1 = GHC.Parser.Lexer.mkPState dynFlags stringBuffer realSrcLoc
        parseResult = GHC.Parser.Lexer.unP GHC.Parser.parseModule pState1
    in case parseResult of
        GHC.Parser.Lexer.PFailed pState2 -> Left . Errors
            $ GHC.Parser.Lexer.getErrorMessages pState2 dynFlags
        GHC.Parser.Lexer.POk _ m -> Right $ Module m
