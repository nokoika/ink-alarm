import qualified Test.Lib (test)
import qualified Test.ICal (test)
import qualified Test.SplaApi (test)
import qualified Test.Query (test)
import qualified Test.Date (test)
import qualified Test.Filter (test)
import qualified Test.Translation (test)
import Prelude (IO)

main :: IO ()
main = do
  Test.Lib.test
  Test.ICal.test
  Test.SplaApi.test
  Test.Query.test
  Test.Date.test
  Test.Filter.test
  Test.Translation.test
