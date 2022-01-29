-- |

module Logger where
import System.IO (hFlush, stdout)

logStdOut :: String -> IO ()
logStdOut text = do
  putStrLn text
  hFlush stdout
