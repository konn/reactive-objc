{-# LANGUAGE RecordWildCards #-}
module Main where
import           Control.Applicative                   ((<$>))
import           Control.Monad                         (when)
import           Data.List                             (intersperse, nub)
import           Data.Maybe
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Distribution.Cab.Sandbox
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import qualified GHC.Paths                             as Path
import qualified System.Directory                      as D
import           System.Environment                    (getArgs)
import           System.Environment                    (withArgs)

build :: FilePath
build = "_build"

data AppSetting = AppSetting { app      :: String
                             , ldFlags  :: [String]
                             , packDBs  :: [FilePath]
                             , includes :: [FilePath]
                             } deriving (Read, Show, Eq, Ord)

main :: IO ()
main = do
  cab <- head . filter ((== ".cabal") . takeExtension) <$> (D.getDirectoryContents =<< D.getCurrentDirectory)
  packdesc <- readPackageDescription silent cab
  msandbox <- getSandbox
  let deps0 = buildDepends $ packageDescription packdesc
      exes  = map (condTreeData . snd) $ condExecutables packdesc
      name  = fst $ head $ condExecutables packdesc
      deps1 = condTreeConstraints $ snd $ head $ condExecutables packdesc
      dbs   = maybeToList msandbox
      incs  = map (</> "include") [Path.libdir, Path.libdir ++ "/../.."]
  if length exes /= 1
    then error "only one executable is currently supported"
    else
    let targ = head exes
        deps = deps0 ++ deps1 ++ targetBuildDepends (buildInfo targ)
        packs = prefixing "-package" $ nub $ map getPackageName deps
        frams = prefixing "-framework" $ frameworks $ buildInfo targ
        opts  = ["-optl-ObjC", "-threaded"]
        setting = AppSetting { app = name
                             , ldFlags = packs ++ frams ++ opts
                             , packDBs = dbs
                             , includes = incs
                             }
    in shakeArgs shakeOptions{shakeFiles=build} $ buildWith setting

prefixing :: a -> [a] -> [a]
prefixing _   [] = []
prefixing str xs = str : intersperse str xs

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName name) _) = name

buildWith :: AppSetting -> Rules ()
buildWith AppSetting {..} = do
  let dbs = prefixing "-package-db" packDBs
  want [build </> app <.> "app"]
  phony "clean" $ do
    putNormal "cleaning..."
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "" ["//*_objc.m", "//*_objc.h", "//*_stub.h", "//*.hi"]

  build </> app <.> "app" *> \out -> do
    putNormal "setting up for bundle..."
    putNormal "compiling xcodeproj..."
    () <- cmd "xcodebuild -project" ("xcode_proj" </> app </> app <.> "xcodeproj")
    () <- cmd "mv" "-f" ("xcode_proj" </> app </> "build/Release" </> app <.> "app") out
    need [build </> app]
    copyFile' (build </> app) (out </> "Contents/MacOS" </> app)

  build </> app *> \out -> do
    hss <- getDirectoryFiles "" ["//*.hs"]
    let objs = [build </> hs -<.> "o" | hs <- hss, hs `notElem` ["Setup.hs", "Builder.hs"]]
    putNormal $ "needed objects: " ++ unwords objs
    need objs
    addObs <- getDirectoryFiles "" ["_build//*_objc.o"]
    putNormal $ "linking executable with: " ++ unwords (objs ++ addObs)
    putNormal $ "link: " ++ unwords (["ghc -o", out] ++ objs ++ addObs ++ ldFlags ++ dbs)
    cmd "ghc -o" out (objs ++ addObs) ldFlags dbs

  "//*.hi" *> \out -> need [build </> out -<.> "o"]

  "_build//*.o" *> \out -> do
    putNormal $ "building object: " ++ out
    let hs = dropDirectory1 $ out -<.> "hs"
        dep = out -<.> "dep"
        hi  = out -<.> "hi"
        obcBase = dropExtension (dropDirectory1 out) ++ "_objc"
        obcm = obcBase <.> "m"
        obch = obcBase <.> "h"
    putNormal $ "generating deps for: " ++ hs
    command_ [] "ghc" $ ["-M", "-dep-suffix", "", "-dep-makefile", dep, hs] ++ dbs
    needMakefileDependencies dep
    () <- cmd "ghc" "-c" hs "-o" out dbs
    gen'd <- doesFileExist obcm
    when gen'd $ do
      putNormal $ "compiling gen'd file: " ++ obcm
      () <- cmd "mv" obcm (build </> obcm)
      () <- cmd "mv" obch (build </> obch)
      () <- cmd "cc" "-fobjc-arc"
        (map ("-I"++) includes)
        "-c -o" (build </> obcBase <.> "o") (build </> obcm)
      cmd "rm" "-f" $ dropExtension (dropDirectory1 out) ++ "_stub" <.> "h"
    putNormal $ "built: " ++ out
