{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word (Word8)
import Data.Char (isSpace, isDigit, chr)
import Data.Int (Int64)
import System.Environment (getArgs)
import PGM(Graymap(..))
import Control.Applicative ((<$>))

-- | A parse state
data ParseState = ParseState
  { string :: L.ByteString -- ^ string to be parsed
  , offset :: Int64        -- ^ offset of the original string
  } deriving (Show)

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

w2c :: Word8 -> Char
w2c = chr . fromIntegral

notWhite :: Char -> Bool
notWhite = (`L8.notElem` " \r\n\t")

-- | Returns the current parsing state
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

-- | Replaces the current parsing state
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- | Terminates parsing and returns an error
bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

-- | Chains parsers together
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState =
      case runParse firstParser initState of
        Left errMessage -> Left errMessage
        Right (firstResult, newState) ->
          runParse (secondParser firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

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

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
      then parseByte ==> \b -> (b :) <$> parseWhile p
      else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat =
  parseWhileWith w2c isDigit ==> \digits ->
    if null digits
      then bail "no more input"
      else let n = read digits
           in if n < 0
                then bail "integer overflow"
                else identity n

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st {offset = offset st + L.length h, string = t}
    in putState st' ==>& assert (L.length h == n') "end of input" ==>&
       identity h

parseP5 :: Parse Graymap
parseP5 =
  parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
  assert (header == "P5") "invalid raw header" ==>&
  parseNat ==> \width -> skipSpaces ==>&
  parseNat ==> \height -> skipSpaces ==>&
  parseNat ==> \maxGrey ->
  parseByte ==>&
  parseBytes (width * height) ==> \bitmap ->
  identity (Graymap width height maxGrey bitmap)

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      let image = runParse parseP5 (ParseState src 0)
      print image
    _ -> putStrLn "Usage: PGM3 <image>"
