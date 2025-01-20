{-# LANGUAGE TemplateHaskell #-}
import           Control.Exception                  (throwIO)
import           Control.Monad
import           Data.Char                          (isSpace)
import           Data.List                          (dropWhile, find, findIndex, isPrefixOf, tails)
import           Data.Maybe                         (fromJust)
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo
import           PseudoMacros
import           System.Directory
import           System.Exit
import           System.Process                     (readProcess, system)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {
      preConf = buildRustLib
    }

buildRustLib :: Args -> a -> IO HookedBuildInfo
buildRustLib _ flags = do

    let file = $__FILE__
    let pathToDistNewstyle = take (fromJust $ findIndex (isPrefixOf "dist-newstyle") (tails file)) file

    isNotDependency <- doesFileExist (pathToDistNewstyle ++ "rust-wrapper/Cargo.toml")

    pathToRustWrapper <- if isNotDependency
        then return pathToDistNewstyle
        else do
          contents <- listDirectory (pathToDistNewstyle ++ "dist-newstyle/src/")
          print $ contents
          depLibs <- filterM (\p -> do
            let prefixCond = isPrefixOf "zkfold-pr" p
            dirCond <- doesDirectoryExist (pathToDistNewstyle ++ "dist-newstyle/src/" ++ p)
            return $ dirCond && prefixCond) contents
          print $ depLibs
          return $ pathToDistNewstyle ++ "dist-newstyle/src/" ++ (head depLibs) ++ "/"

    buildResult <- system ("cargo +nightly build --release " ++
      "--manifest-path " ++ pathToRustWrapper ++ "rust-wrapper/Cargo.toml " ++
      "--artifact-dir=" ++ pathToDistNewstyle ++ "libs/ -Z unstable-options"
      )

    case buildResult of
      ExitSuccess          -> return ()
      ExitFailure exitCode -> do
        throwIO $ userError $ "Build rust library failed with exit code " <> show exitCode

    return emptyHookedBuildInfo
