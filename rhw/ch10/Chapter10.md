# Chapter 10

In this chapter we are going to create a binary [Portable Gray Map
(PGM)](https://en.wikipedia.org/wiki/Netpbm_format) parser. This repository
includes an example of a PGM as `test.pgm`. The content of this file is shown
bellow:

```
P5
4 4
255
...
```

The PGM format is structured as follows:
* Magic number identifying the file type. For binary PGM the magic number is
  "P5".
* The width and height separated by one space.
* The maximum gray value. Must be a value from 0 to 255.
* The raster image in binary format.

We're going to use the following data type to represent PGM.

```{haskell}
import qualified Data.ByteString.Lazy as L

-- | A portable gray map (PGM)
data Graymap = Graymap
  { grayWidth :: Int
  , grayHeight :: Int
  , grayMax :: Int
  , grayData :: L.ByteString
  } deriving (Eq)

instance Show Graymap where
  show (Graymap w h m _) =
    "Graymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
```

We're using `ByteString` instead of `String` because it's a time and space
efficient type for storing binary data. Instead of deriving `Show` we are
defining our own instance because we don't want the binary data to be shown,
which could be lengthy and it would be unreadable anyway, since it is an image.

At this point we should be able to create a `Graymap` on `ghci`:

```
ghci> Graymap 640 480 255 L.empty
Graymap 640x480 255
```

A PGM file can have comments, spaces and tabs but for we're going to ignore all
that to make parsing easier.

## First Parser

For our first parser we're just going to use case expressions because it's the
most obvious solution. For each line of a PGM file we're going to use a case
expression that will bind an element of the PGM and the remainder of the file.
In the end all elements should be binded and we can construct a `Graymap`. But
first we will need some auxiliary functions.

We're not going to store the magic number but we still have to guarantee that
the first line is the magic number `P5`. For that we're going to use
`stripPrefix`. Given two strings returns the remainder of the second string if
the first is a prefix of the second.

```
ghci> :set -XOverloadedStrings
ghci> import Data.ByteString.Lazy
ghci> stripPrefix "foo" "foobar"
Just "bar"
ghci> stripPrefix "" "foobar"
Just "foobar"
ghci> stripPrefix "foo" "quux"
Nothing
```

So basically we're going to call `stripPrefix "P5"`. The PGM format allows space
characters (e.g., `' '`, `'\t'`,`'\n'`) after the magic number and other
elements as well. We need a function to skip spaces:

```{haskell}
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)

skipSpaces :: L.ByteString -> L.ByteString
skipSpaces = L8.dropWhile isSpace
```

Given a string all prefixed space characters are dropped:

```
ghci> :set -XOverloadedStrings
ghci> skipSpaces "          foo"
"foo"
ghci> skipSpaces "\t\n\r\f\vfoo"
"foo"
ghci> skipSpaces "bar          "
"bar          "
ghci> skipSpaces "             "
""
```

For the width, height and maximum gray value we will use a function to parse
natural numbers:

```{haskell}
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

readNat :: L.ByteString -> Maybe (Int, L.ByteString)
readNat s =
  case L8.readInt s of
    Nothing -> Nothing
    j@(Just (n, _))
      | n <= 0 -> Nothing
      | otherwise -> j
```

If the given string starts with a natural number this functions returns a pair
with the natural number and the remainder of the string:

```
ghci> :set -XOverloadedStrings
ghci> readNat "5bar"
Just (5,"bar")
ghci> readNat "42cards"
Just (42,"cards")
gchi> readNat "cards"
Nothing
gchi> readNat "0cards"
Nothing
```

Finally we need a function to read bytes:

```{haskell}
import qualified Data.ByteString.Lazy as L

readBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
readBytes n str =
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
  in if L.length prefix < count
       then Nothing
       else Just both
```

Given a number of `N` bytes and a string, this function splits the string at
`N` bytes into a pair, if the length of the string is lower than `N`:

```
gchi> :set -XOverloadedStrings 
gchi> readBytes 3 "abcdef"
Just ("abc","def")
gchi> readBytes 6 "abcdef"
Just ("abcdef","")
gchi> readBytes 0 "abcdef"
Just ("","abcdef")
gchi> readBytes (-2) "abcdef"
Just ("","abcdef")
gchi> readBytes 8 "abcdef"
Nothing
```

With these auxiliary functions we can make our first parser:

```{haskell}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as L

parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
parseP5 s =
  case L.stripPrefix "P5" s of
    Nothing -> Nothing
    Just s1 ->
      case readNat (skipSpaces s1) of
        Nothing -> Nothing
        Just (width, s2) ->
          case readNat (skipSpaces s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case readNat (skipSpaces s3) of
                Nothing -> Nothing
                Just (maxGray, s4)
                  | maxGray > 255 -> Nothing
                  | otherwise ->
                    case readBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case readBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Graymap width height maxGray bitmap, s6)
```

Given a string that represents a PGM file we return a `Graymap` if the string
corresponds to the PGM format. As mentioned earlier we are using the most
obvious solution with multiple case expressions. Each case expression binds a
pair with an element of the PGM (e.g., magic number, width, height) and the
remainder of the string to be used to continue parsing. At the end we can
simply use all binded elements to construct the `Graymap`.

The parser algorithm goes like this:

 - Expect the magic number `P5`.
 - Skip spaces and expect a natural number corresponding to the width.
 - Skip spaces and expect a natural number corresponding to the height.
 - Skip spaces and except a number from 0 to 255 corresponding to the maximum
   gray value.
 - Expect one byte corresponding to a white space or new line.
 - Expect a byte string of length `width * height` corresponding to the binary
   data.
 - If everything goes as expected return `Just (Graymap, s)` where `s` is the
   remainder of the string or `Nothing` otherwise.

We should be able to test the parser:

```
gchi> :set -XOverloadedStrings                           
gchi> parseP5 "P5\n2 2\n255\n1234" 
Just (Graymap 2x2 255,"")     
gchi> parseP5 "P5\n2 2\n255\n12345678"
Just (Graymap 2x2 255,"5678")
gchi> parseP5 "P5\n2 2\n255\n123"               
Nothing                     
gchi> parseP5 "P5    2    2    255 1234"
Just (Graymap 2x2 255,"")   
```

To finish we can also implement a main function that receives and prints a PGM
file:

```{haskell}
import qualified Data.ByteString.Lazy as L
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      src <- L.readFile x
      print $ parseP5 src
    _ -> putStrLn "PGM file missing"
```

The application takes a PGM file and prints it:

```{bash}
$runhaskell PGM1.hs test.pgm
Just (Graymap 8x8 255,"\n")
```

The issue with this implementation is that it's very hard to read. The code
looks like a staircase that would go offscreen if it were more complex. The
next parser implementation will fix this issue.

## Second parser

You may have noticed that in the previous `parseP5` function every step looked
as such:

```{haskell}
case f a of
  Nothing -> Nothing
  Just b  ->
    case g b of
      ...
```

This is what's going on:

 * A function `f` takes `a` and returns `Maybe b`.
 * If it's `Nothing` the algorithm ends.
 * If it's `Just b` another function `g` takes `b` and returns `Maybe c`, and
   so on.

We can abstract this code with a function:

```{haskell}
chain :: Maybe a -> (a -> Maybe b) -> Maybe b
chain Nothing _   = Nothing
chain (Just v) f  = f v
```

With this function we can chain functions that take a `Maybe` and return
another `Maybe`, so we can reduce our code to one line:

```
ghci> f a = Just (a + 1)
ghci> g b = Just (b + 2)
ghci> chain (f 1) g
Just 4
```

Naturally it doesn't just let you chain two functions, you can chain as many as
you want:

```
ghci> h c = Just (c + 3)
ghci> j d = Just (d + 4)
ghci> chain (chain (chain (f 1) g) h) j
Just 11
```

However this code can get ugly with too many chained functions so perhaps it is
best to use suffix style:

```
ghci> f 1 `chain` g `chain` h `chain` j
Just 11
```

But if we're going to use suffix style most of the time our function might as
well be an operator:

```{haskell}
>>? :: Maybe a -> (a -> Maybe b) -> Maybe b
>>? Just v >>? f  = f v
>>? Nothing _   = Nothing
```

We can execute the same code like so:

```
ghci> f 1 >>? g >>? h >>? j
Just 11
``` 

So the point of all this is that we can refactor our `parseP5` function by
chaining everything:

```{haskell}
import qualified Data.ByteString.Lazy as L

parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
parseP5 s =
    stripPrefix "P5" s                        >>?
    \s1 -> readNat (skipSpaces s1)            >>?
    \(width, s2) -> readNat (skipSpaces s2)   >>?
    \(height, s3) ->  readNat (skipSpaces s3) >>?
    \(maxGray, s4) -> readBytes 1 s4          >>?
    (readBytes (width * height) . snd)        >>?
    \(bitmap, s5) -> Just (Graymap width height maxGray bitmap, s5)
```

Chaining functions is so common in Haskell that the standard library has
generic function for this:

```{haskell}
import qualified Data.ByteString.Lazy as L

parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
parseP5 s =
    stripPrefix "P5" s                         >>=
    \s1            -> readNat (skipSpaces s1)  >>=
    \(width, s2)   -> readNat (skipSpaces s2)  >>=
    \(height, s3)  ->  readNat (skipSpaces s3) >>=
    \(maxGray, s4) -> readBytes 1 s4           >>=
    (readBytes (width * height) . snd)         >>=
    \(bitmap, s5)  -> Just (Graymap width height maxGray bitmap, s5)
```

The `>>=` is part of the `Monad` class, which we'll talk later. For now we only
need to know that the `Maybe` implementation of `>>=` is identical to our
`>>?`:

```{haskell}
instance Monad Maybe where
    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing
```

You can also use the `do` notation to make the code cleaner:

```{haskell}
import qualified Data.ByteString.Lazy as L

parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
parseP5 s = do
    s1            <- stripPrefix "P5" s
    (width, s2)   <- readNat (skipSpaces s1)
    (height, s3)  <- readNat (skipSpaces s2)
    (maxGray, s4) <- readNat (skipSpaces s3)
    (_, s5)       <- readBytes 1 s4
    (bitmap, s6)  <- readBytes (width * height) s5
    Just (Graymap width height maxGray bitmap, s6)
```

The `do` notation is easier to indent and shows the function execution and it's
result in the same line instead of the line bellow. If you're wondering if the
`do` notation is related to the `Monad` class the answer is yes, it is. We will
detail the `do` notation later as well.

Our parser looks much cleaner now but we can further optimize the code. Since
we are explicitly passing around tuples we may have to change a lot of code to
extend our parser. For example, if we wish store how many bytes were parsed, in
order to report a parsing error, we have to change every tuple to include the
number of parsed bytes. Our next parser will be easier to mantain and extend.

## Third Parser

Our previous parser used pattern matching to pull parsed data and what was left
to be parsed. However that proved to be hard to extend. To fix this issue we
are going to start this parser from scratch. 

First we're going to use an algebraic data type to represent parsing state:

```{haskell}
data ParseState = ParseState
  { string :: L.ByteString
  , offset :: Int64
  } deriving (Show)
```

This data type stores the current residual string and the offset of the
original string. If we need to further extend the parser the return type will
remain the same. We are also giving the parsing state a name so the code
becomes easier to understand.

The most obvious way to update our parser would be to replace `ByteString` with
`ParseState` like so:

```{haskell}
parse :: ParseState -> Maybe (a, ParseState)
parse = undefined
```

For example, we could update the natural number parser to fit the `ParseState`:

```{haskell}
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

readNat :: ParseState -> Maybe (Int, ParseState)
readNat state =
  case L8.readInt (string state) of
    Nothing -> Nothing
    Just (n, s)
      | n <= 0 -> Nothing
      | otherwise -> Just (n, newState)
        where newState  = ParseState s newOffset
              newOffset = offset state + length (show n)
```
