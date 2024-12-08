module Lib (someFunc) where

import Prelude (IO, putStrLn)

someFunc :: IO ()
someFunc = do
  putStrLn "Hello, world!"
