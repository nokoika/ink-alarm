module App.Error
  ( AppError (..),
    toHttpError,
  )
where

import Data.Text (Text)
import Network.HTTP.Types.Status
import Prelude (Eq, Show)

data AppError
  = InvalidQuery Text
  | ApiFetchError Text
  | InternalError Text
  | NotFound Text
  deriving (Show, Eq)

toHttpError :: AppError -> (Status, Text)
toHttpError err = case err of
  InvalidQuery msg -> (badRequest400, msg)
  ApiFetchError msg -> (badGateway502, msg)
  InternalError msg -> (internalServerError500, msg)
  NotFound msg -> (notFound404, msg)
