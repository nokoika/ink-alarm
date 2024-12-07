import qualified LibTest (test)
import qualified ICalTest (test)
import qualified SplaApiTest (test)
import qualified QueryTest (test)
import qualified FilterTest (test)
import Prelude (IO)

main :: IO ()
main = do
  LibTest.test
  ICalTest.test
  SplaApiTest.test
  QueryTest.test
  FilterTest.test
