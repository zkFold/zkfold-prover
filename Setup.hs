{-# LANGUAGE TemplateHaskell #-}
import           Control.Exception                  (throwIO)
import           Control.Monad
import           Data.Char                          (isSpace)
import           Data.Maybe                         (fromJust)
import           Data.List                          (dropWhile, isPrefixOf, tails, findIndex, find)
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

infi = do
  infi

buildRustLib :: Args -> a -> IO HookedBuildInfo
buildRustLib _ flags = do

    let file = $__FILE__
    let pathToDistNewstyle = take (fromJust $ findIndex (isPrefixOf "dist-newstyle") (tails file)) file

    isNotDependency <- doesFileExist (pathToDistNewstyle ++ "rust-wrapper/Cargo.toml")
    
    -- infi
    print $ file
    print $ pathToDistNewstyle

    pathToRustWrapper <- if isNotDependency
        then return pathToDistNewstyle
        else do
          contents <- listDirectory (pathToDistNewstyle ++ "src/")
          print $ contents
          let depLib = fromJust $ find (isPrefixOf "zkfold-pr") contents
          print $ depLib
          return $ pathToDistNewstyle ++ "src/" ++ depLib

    putStrLn $ pathToRustWrapper

    buildResult <- system ("cargo +nightly build --release " ++ 
      "--manifest-path " ++ pathToRustWrapper ++ "rust-wrapper/Cargo.toml " ++ 
      "--artifact-dir=" ++ pathToRustWrapper ++ "libs/ -Z unstable-options"
      )

    case buildResult of
      ExitSuccess          -> return ()
      ExitFailure exitCode -> do
        throwIO $ userError $ "Build rust library failed with exit code " <> show exitCode

    return emptyHookedBuildInfo
