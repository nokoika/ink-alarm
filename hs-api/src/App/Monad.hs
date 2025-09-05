{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad
  ( AppM (..),
    runAppM,
  )
where

import App.Context (AppContext)
import App.Error (AppError)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT)
import Prelude (Applicative, Either, Functor, IO, Monad)

newtype AppM a = AppM
  { unAppM :: ExceptT AppError (ReaderT AppContext IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppContext, MonadError AppError)

runAppM :: AppContext -> AppM a -> IO (Either AppError a)
runAppM ctx appM =
  runReaderT
    (runExceptT (unAppM appM))
    ctx
