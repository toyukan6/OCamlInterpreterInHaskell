module Main where

import Text.ParserCombinators.Parsec (parse)

import Parser (parseProgram)

parser :: String -> String
parser input =
    either show show $ parse parseProgram "OCaml" input

main :: IO ()
main = do
  putStrLn "ready..."
  input <- getLine
  if input /= "quit"
  then do putStrLn . parser $ input
          main
  else return ()