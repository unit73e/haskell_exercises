{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word (Word8)
import Data.Char (isSpace)
import Data.Int (Int64)
import System.Environment (getArgs)
import PGM(Graymap(..))

-- | A parse state
data ParseState = ParseState
  { string :: L.ByteString -- ^ string to be parsed
  , offset :: Int64        -- ^ offset of the original string
  } deriving (Show)

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

-- | Set a new offset
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

-- | Injects a value into a parser
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
      Just (byte, remainder) -> putState newState ==> \_ -> identity byte
        where newState = initState {string = remainder, offset = newOffset}
              newOffset = offset initState + 1

-- | Returns the current parsing state
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

-- | Replaces the current parsing state
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- | Terminates parsing and returns an error
bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

-- | Chains parsers together
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState =
      case runParse firstParser initState of
        Left errMessage -> Left errMessage
        Right (firstResult, newState) ->
          runParse (secondParser firstResult) newState

-- | Parses a PGM string
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result

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
  | prefix `L8.isPrefixOf` str = Just suffix
  | otherwise = Nothing
  where
    suffix = L.drop (L.length prefix) str

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

-- | Returns the given string without prefixed spaces.
--
-- Examples:
-- > skipSpaces "          foo" == "foo"
-- > skipSpaces "\t\n\r\f\vfoo" == "foo"
-- > skipSpaces "bar          " == "bar          "
-- > skipSpaces "             " == ""
skipSpaces :: L.ByteString -> L.ByteString
skipSpaces = L8.dropWhile isSpace

-- | Translates a PGM string into a @Graymap@ or returns @Nothing@
-- if the string is not a PGM
parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
parseP5 s = do
    s1            <- stripPrefix "P5" s
    (width, s2)   <- readNat (skipSpaces s1)
    (height, s3)  <- readNat (skipSpaces s2)
    (maxGray, s4) <- readNat (skipSpaces s3)
    (_, s5)       <- readBytes 1 s4
    (bitmap, s6)  <- readBytes (width * height) s5
    Just (Graymap width height maxGray bitmap, s6)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      let image = parseP5 src
      print image
    _ -> putStrLn "Usage: PGM3 <image>"
