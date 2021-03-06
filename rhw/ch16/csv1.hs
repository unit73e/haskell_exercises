import Prelude hiding (getContents)

import Text.Parsec
import Text.Parsec.Text.Lazy
import Data.Text.Lazy.IO (getContents)

-- | A CSV file contains 0 or more lines, each of which is terminated
-- by the end-of-line character (eol).
csvFile :: Parser [[String]]
csvFile = do
  result <- many line
  eof
  return result

-- | Each line contains 1 or more cells, separated by a comma
line :: Parser [String]
line = do
  result <- cells
  _ <- eol
  return result

-- | Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: Parser [String]
cells = do
  first <- cellContent
  next <- remainingCells
  return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: Parser [String]
remainingCells =
  (char ',' >> cells) -- Found comma?  More cells coming
   <|>
  return [] -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: Parser String
cellContent = many (noneOf ",\n")

-- The end of line character is \n
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
