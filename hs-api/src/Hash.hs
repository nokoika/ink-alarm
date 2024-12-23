module Hash (sha256Hash) where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude (String, ($))

sha256Hash :: String -> String
sha256Hash input = BS.unpack $ BA.convertToBase BA.Base16 digest
  where
    digest :: Crypto.Digest Crypto.SHA256
    digest = Crypto.hash $ TE.encodeUtf8 $ T.pack input
