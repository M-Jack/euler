
import System.Timeout
import System.Environment
import System.TimeIt
import Control.Monad
import Euler

main :: IO ()
main = do
  problem <- liftM (read . head)  getArgs 
  maybeSol <- timeout (5 * minute) $ timeIt $ return $! solve problem
  maybe (putStrLn "TimeOut") putStrLn $ fmap show maybeSol 
       where minute = 10 ^ 6 * 60
