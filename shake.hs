import           Development.Shake
import           Development.Shake.Cabal
import           Development.Shake.Clean
import           Development.Shake.ClosureCompiler
import           Development.Shake.FilePath
import           System.Directory
import qualified System.IO.Strict                  as Strict

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake", shakeLint = Just LintBasic } $ do
    want [ "docs/index.html", "README.md" ]

    "clean" ~> do
        cleanHaskell
        removeFilesAfter "docs" ["//*"]
        removeFilesAfter ".shake" ["//*"]

    "README.md" %> \out -> do
        hs <- getDirectoryFiles "" ["src//*.hs"]
        yaml <- getDirectoryFiles "" ["//*.yaml"]
        cabal <- getDirectoryFiles "" ["//*.cabal"]
        mad <- getDirectoryFiles "" ["//*.mad"]
        html <- getDirectoryFiles "" ["web-src//*.html"]
        css <- getDirectoryFiles "" ["web-src//*.css"]
        need $ hs <> yaml <> cabal <> mad <> html <> css
        (Stdout out') <- cmd ["poly", "-c", ".", "-e", "README.md", "-e", "TODO.md", "-e", "docs", "-e", "Justfile"]
        file <- liftIO $ Strict.readFile out
        let header = takeWhile (/= replicate 79 '-') $ lines file
        let new = unlines header ++ out' ++ "```\n"
        liftIO $ writeFile out new

    "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/hot-takes-0.1.0.0/x/hot-takes/opt/build/hot-takes/hot-takes.jsexe/all.js" %> \_ -> do
        need . snd =<< getCabalDepsA "hot-takes.cabal"
        -- check the hot-takes.mad file so we don't push anything wrong
        unit $ cmd ["bash", "-c", "madlang check mad-src/hot-takes.mad > /dev/null"]
        command [RemEnv "GHC_PACKAGE_PATH"] "cabal" ["new-build"]

    googleClosureCompiler ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/hot-takes-0.1.0.0/x/hot-takes/opt/build/hot-takes/hot-takes.jsexe/all.js", "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/hot-takes-0.1.0.0/x/hot-takes/opt/build/hot-takes/hot-takes.jsexe/all.js"] "docs/all.min.js"

    "docs/styles.css" %> \out -> do
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        need ["web-src/styles.css"]
        copyFile' "web-src/styles.css" out

    "docs/index.html" %> \out -> do
        need ["docs/all.min.js", "web-src/index.html", "docs/styles.css"]
        copyFile' "web-src/index.html" out
