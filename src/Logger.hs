-- |

module Logger where
import ClassyPrelude

logStdOut :: Text -> IO ()
logStdOut text = do
  putStrLn text
  hFlush stdout
