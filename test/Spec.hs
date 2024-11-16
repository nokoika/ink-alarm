import qualified LibTest (test)
import qualified ICalTest (test)
import Prelude (IO)

main :: IO ()
main = do
  LibTest.test
  ICalTest.test
