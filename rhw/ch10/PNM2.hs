import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import System.Environment (getArgs)

data Greymap = Greymap
  { greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) =
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str =
    Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s =
  case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest)
      | num <= 0 -> Nothing
      | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str =
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
  in if L.length prefix < count
       then Nothing
       else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      let image = parseP5 src
      print image
    _ -> putStrLn "Usage: PNM <image>"
