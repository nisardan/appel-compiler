module Main (main) where

import Lexer (tigerLex)
import System.IO (getContents)

main :: IO ()
main = do
  contents <- getContents
  putStrLn . show $ tigerLex contents
