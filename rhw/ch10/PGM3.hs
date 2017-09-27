{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word (Word8)
import Data.Char (isSpace, isDigit, chr)
import Data.Int (Int64)
import System.Environment (getArgs)
import PGM(Graymap(..))
import Control.Applicative ((<$>))
import Control.Monad (ap, liftM)

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

assert :: Bool -> String -> Parse ()
assert True _ = return ()
assert False err = fail err

instance Functor Parse where
  fmap = liftM

instance Applicative Parse where
  pure = return
  (<*>) = ap

instance Monad Parse where
  m >>= k =
    Parse $ \s ->
      case runParse m s of
        Left errMessage -> Left errMessage
        Right (a, s') -> runParse (k a) s'
  return a = Parse $ \s -> Right (a, s)
  fail err =
    Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

parseByte :: Parse Word8
parseByte = do
  initState <- getState
  case L.uncons (string initState) of
    Nothing -> fail "no more input"
    Just (byte, remainder) -> do
      let newState = initState {string = remainder, offset = newOffset}
          newOffset = offset initState + 1
      putState newState
      return byte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = do
  mp <- fmap p <$> peekByte
  if mp == Just True
    then parseByte >>= \b -> (b :) <$> parseWhile p
    else return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = do
  digits <- parseWhileWith w2c isDigit
  if null digits
    then fail "no more input"
    else let n = read digits
         in if n < 0
              then fail "integer overflow"
              else return n

parseBytes :: Int -> Parse L.ByteString
parseBytes n = do
  st <- getState
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st {offset = offset st + L.length h, string = t}
  putState st'
  assert (L.length h == n') "end of input"
  return h

parseP5 :: Parse Graymap
parseP5 = do
  header <- parseWhileWith w2c notWhite
  skipSpaces
  assert (header == "P5") "invalid raw header"
  width <- parseNat
  skipSpaces
  height <- parseNat
  skipSpaces
  maxGrey <- parseNat
  _ <- parseByte
  bitmap <- parseBytes (width * height)
  return (Graymap width height maxGrey bitmap)

skipSpaces :: Parse ()
skipSpaces = do
  _ <- parseWhileWith w2c isSpace
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      let image = runParse parseP5 (ParseState src 0)
      print image
    _ -> putStrLn "Usage: PGM3 <image>"
