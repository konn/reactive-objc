module Builder where
import Control.Monad              (when)
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

app :: FilePath
app = "Reactive"

build :: FilePath
build = "_build"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=build} $ do
  want [build </> app <.> "app"]
  phony "clean" $ do
    putNormal "cleaning..."
    removeFilesAfter "_build" ["//*"]
    removeFilesAfter "" ["//*_objc.m", "//*_objc.h"]

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
    cmd "ghc -o" out (objs ++ addObs) ldFlags

  "_build//*.o" *> \out -> do
    putNormal $ "building object: " ++ out
    let hs = dropDirectory1 $ out -<.> "hs"
        dep = out -<.> "dep"
        obcBase = dropExtension (dropDirectory1 out) ++ "_objc"
        obcm = obcBase <.> "m"
    putNormal $ "generating deps for: " ++ hs
    command_ [] "ghc" ["-M", "-dep-suffix", "", "-dep-makefile", dep, hs]
    needMakefileDependencies dep
    () <- cmd "ghc" "-c" hs "-o" out
    gen'd <- doesFileExist obcm
    when gen'd $ do
      putNormal $ "compiling gen'd file: " ++ obcm
      cmd "cc" "-fobjc-arc"
        "-I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64/usr/lib/ghc-7.8.3/include"
        "-I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64/usr/lib/ghc-7.8.3/../../includes"
        "-c -o" (build </> obcBase <.> "o") obcm
    putNormal $ "built: " ++ out



ldFlags :: [String]
ldFlags = [ "-package", "template-haskell"
          , "-package", "language-c-quote"
          , "-package", "language-c-inline"
          , "-package", "sodium"
          , "-package", "lens"
          , "-framework", "Cocoa"
          , "-optl-ObjC", "-threaded"]
