import System.Environment(getArgs)
import System.Exit(exitFailure)
import Control.Monad(when)

main :: IO ()
main = do
   args <- getArgs

   when (length args /= 2) $ do
      putStrLn "Syntax: passwd-al filename uid"
      exitFailure

   content <- readFile $ head args
   let uid = read $ args !! 1
   let username = findByUID content uid

   case username of
      Just a  -> putStrLn a
      Nothing -> putStrLn "Not found"

findByUID :: String -> Integer -> Maybe String
findByUID content uid = lookup uid al
   where al = map parseline . lines $ content

parseline :: String -> (Integer, String)
parseline line = (uid, name)
   where l     = split ':' line
         name  = head l
         uid   = read $ l !! 2

split :: Eq a => a -> [a] -> [[a]]
split _ []      = [[]]
split delim str =
   before : case remainder of
                 []     -> []
                 (_:xs) -> split delim xs
   where (before, remainder) = span (/= delim) str
