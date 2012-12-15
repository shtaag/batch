import Test.Hspec.Runner
import Test.QuickCheck
import CategorizeSpec (spec)
import System.Exit
import Control.Applicative

main = exit <$> hspecWith option spec

option :: Config
option = defaultConfig { configVerbose = True}

exit :: Summary -> IO ()
exit (Summary success failure)
  | failure > 0  = exitFailure
  | success == 0 = exitFailure
  | otherwise    = exitSuccess
  
