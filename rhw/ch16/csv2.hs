import Prelude hiding (getContents)

import Text.Parsec
import Text.Parsec.Text.Lazy
import Data.Text.Lazy.IO (getContents)

csvFile :: Parser [[String]]
csvFile = endBy line eol

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser String
cell = many (noneOf ",\n")

eol :: Parser Char
eol = char '\n'

main :: IO ()
main = do
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> mapM_ print r
