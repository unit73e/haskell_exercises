{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import System.Environment (getArgs)

-- | A portable grey map (PGM)
data Greymap = Greymap
  { greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) =
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

-- | Return the suffix of the second string if its prefix matches
-- the entire first string.
--
-- Examples:
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix ""    "baz"    == Just "baz"
-- > stripPrefix "foo" "quux"   == Nothing
stripPrefix :: L.ByteString -> L.ByteString -> Maybe L.ByteString
stripPrefix prefix str
  | prefix `L8.isPrefixOf` str =
    let afterMagicNumber = L.drop (L.length prefix) str
    in Just $ L8.dropWhile isSpace afterMagicNumber
  | otherwise = Nothing

-- | Reads a natural number from the beginning of the string.
-- A natural number is a positive integer (does not include zero).
-- If there is no natural number at the beginning of the string,
-- returns Nothing, otherwise returns the natural number and the
-- rest of the string.
--
-- > readNat "5cards" == Just (5,"cards")
-- > readNat "cards"  == Nothing
-- > readNat "0cards" == Nothing
readNat :: L.ByteString -> Maybe (Int, L.ByteString)
readNat s =
  case L8.readInt s of
    Nothing -> Nothing
    j@(Just (n, _))
      | n <= 0 -> Nothing
      | otherwise -> j


-- | Returns a tuple where the first element is a prefix of length @n@
-- and the second element is the remainder of the string. If @n@ exceedes
-- the size of the string, return @Nothing@.
--
-- Examples:
-- > readBytes 3 "abcdef"    == Just ("abc","def")
-- > readBytes 6 "abcdef"    == Just ("abcdef","")
-- > readBytes 0 "abcdef"    == Just ("","abcdef")
-- > readBytes (-2) "abcdef" == Just ("","abcdef")
-- > readBytes 8 "abcdef"    == Nothing
readBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
readBytes n str =
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
  in if L.length prefix < count
       then Nothing
       else Just both

-- | Translates a PGM string into a @Greymap@ or returns @Nothing@
-- if the string is not a PGM
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case stripPrefix "P5" s of
    Nothing -> Nothing
    Just s1 ->
      case readNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case readNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case readNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case readBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case readBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Greymap width height maxGrey bitmap, s6)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      let image = parseP5 src
      print image
    _ -> putStrLn "Usage: PNM <image>"
