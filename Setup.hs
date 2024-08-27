import Distribution.Simple
import System.Process
import Control.Monad
import Distribution.Types.HookedBuildInfo
import System.Exit

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preConf = runMyScript

    }

runMyScript :: Args -> a -> IO HookedBuildInfo
runMyScript args flags = do
    putStrLn "Running pre-script..."
    executeShellCommand "source ./run.sh"
    return $ emptyHookedBuildInfo
    
executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check ExitSuccess     = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n