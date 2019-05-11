{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Distribution.Types.PackageDescription
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.System
import System.FilePath
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.String (IsString)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Control.Monad (forM_)

data PcEntry s = PcVar s s | PcDecl s s

genPcContent :: (Monoid s, IsString s) => [PcEntry s] -> s
genPcContent = foldMap makeLine
  where
    makeLine (PcVar k v) = k <> "=" <> v <> "\n"
    makeLine (PcDecl k v) = k <> ": " <> v <> "\n"

-- -----------------------

pcBuildDir :: String
pcBuildDir = "custom.pkgconfig"

pcDir :: String
pcDir = "pkgconfig"

pcFileName :: ForeignLib -> String
pcFileName ForeignLib{..} = flibname ++ ".pc"
  where
    flibname = unUnqualComponentName foreignLibName

genPcForFlib ::
    BuildFlags ->
    PackageDescription -> ForeignLib ->
    LocalBuildInfo -> ComponentLocalBuildInfo ->
    IO ()
genPcForFlib BuildFlags{..} desc flib@ForeignLib{..} lbinfo FLibComponentLocalBuildInfo{..} =
  do
    let
        verbosity = fromFlag buildVerbosity
        Platform _ os = hostPlatform lbinfo
        lunit = componentUnitId
        copydest = fromFlag (installDest defaultInstallFlags) -- NoCopyDest
        paths = absoluteComponentInstallDirs desc lbinfo lunit copydest
        versionNums = foreignLibVersion flib os
        versionStr = Text.intercalate "." $ Text.pack . show <$> versionNums
        flibname = unUnqualComponentName foreignLibName
        destFile = buildDir lbinfo </> pcBuildDir </> pcFileName flib

    TL.writeFile destFile . TB.toLazyText . genPcContent $
      [
        PcVar "flibdir" $ TB.fromString (flibdir paths),
        PcVar "libdir" $ TB.fromString (libdir paths),
        PcVar "flibname" $ TB.fromString flibname,
        PcDecl "Name" $ TB.fromString flibname,
        PcDecl "Description" $ TB.fromString (synopsis desc),
        PcDecl "Version" $ TB.fromText versionStr,
        PcDecl "Libs" "-L${flibdir} -l${flibname}",
        PcDecl "Cflags" "-I${libdir}/include"
      ]

genPcForFlib BuildFlags{..} _ ForeignLib{..} _ _ =
  do
    let verbosity = fromFlag buildVerbosity
        name = (unUnqualComponentName foreignLibName)
    warn verbosity $ "Couldn't get FlibComponentLocalBuildInfo for " ++ name

genPc :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
genPc args bflags desc lbinfo@LocalBuildInfo{..} =
  do
    let verbosity = fromFlag (buildVerbosity bflags)

    notice verbosity "Generating pkgconfig file."

    createDirectoryIfMissingVerbose
        verbosity
        True
        (buildDir </> pcBuildDir)

    forM_ (foreignLibs desc) $ \flib ->
      do
        let uname = foreignLibName flib
            clbinfoList = Map.lookup (CFLibName uname) componentNameMap
            clbinfo = clbinfoList >>= listToMaybe
        maybe (return ()) (genPcForFlib bflags desc flib lbinfo) clbinfo

instPc :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
instPc _ InstallFlags{..} desc@PackageDescription{..} lbinfo@LocalBuildInfo{..} =
  do
    let
        verbosity = fromFlag installVerbosity
        lunit = localUnitId lbinfo
        copydest = fromFlag installDest
        paths = absoluteComponentInstallDirs desc lbinfo lunit copydest
        absPcDir = flibdir paths </> pcDir

    notice verbosity "Installing pkgconfig file."

    createDirectoryIfMissingVerbose
        verbosity
        True
        absPcDir

    forM_ foreignLibs $ \flib ->
      do
        let flibPcFile = pcFileName flib
        installOrdinaryFile
            verbosity
            (buildDir </> pcBuildDir </>flibPcFile)
            (absPcDir </> flibPcFile)

-- ----------------------------

main = defaultMainWithHooks simpleUserHooks { postBuild = genPc, postInst = instPc }
