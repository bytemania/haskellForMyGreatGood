module Capslocker where

import Data.Char
import Control.Monad

main = forever $ do
    l <- getContents
    putStrLn $ map toUpper l
