import           Control.Exception                  (throwIO)
import           Control.Monad
import           Data.Char                          (isSpace)
import           Data.List                          (dropWhile, isPrefixOf)
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo
import           System.Directory
import           System.Exit
import           System.Process                     (readProcess, system)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preConf = buildRustLib
    }

buildRustLib :: Args -> a -> IO HookedBuildInfo
buildRustLib _ flags = do

    buildResult <- system "cargo +nightly cbuild --release --manifest-path rust-wrapper/Cargo.toml"
    case buildResult of
      ExitSuccess          -> return ()
      ExitFailure exitCode -> throwIO $ userError $ "Build rust library failed with exit code " <> show exitCode

    output <- readProcess "rustc" ["--version", "--verbose"] ""
    case filter ("host: " `isPrefixOf`) (lines output) of
      [line] -> do
        let host = dropWhile isSpace $ drop 5 line
            pathToLib = "rust-wrapper/target/" <> host <> "/release/librust_wrapper.so"

        libExist <- doesFileExist pathToLib
        unless libExist $ throwIO $ userError "Can't find rust library"

        copyFile pathToLib "./lib.so"

      _ -> throwIO $ userError "Can't find default rust target"
    return emptyHookedBuildInfo
