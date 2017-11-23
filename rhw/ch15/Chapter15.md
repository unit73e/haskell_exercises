# Chapter 15

## Lifting

Let's use an association list get an instance the following data structure:

```haskell
data MovieReview = MovieReview
  { revTitle :: String
  , revUser :: String
  , revReview :: String
  } deriving (Show)
```

We'll start with the most obvious solution:

```haskell
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) -> Just (MovieReview title user review)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
```

This function returns a `MovieReview` if the given association list contains
non empty values for `title`, `user` and `review` keys. Otherwise returns
`Nothing`.

```haskell
-- Returns a MovieReview since every key and value is present
ghci> simpleReview [("title", Just "A"), ("user", Just "B"), ("review", Just "C")]
Just (MovieReview {revTitle = "A", revUser = "B", revReview = "C" })

-- Returns Nothing since review's value is empty
ghci> simpleReview [("title", Just "A"), ("user", Just "B"), ("review", Just "")]
Nothing

-- Returns Nothing since review is missing
ghci> simpleReview [("title", Just "A"), ("user", Just "B")]
Nothing

-- Returns Nothing since review value is Nothing
ghci> simpleReview [("title", Just "A"), ("user", Just "B"), ("review", Nothing)]
Nothing
```

This version works as expected but suffers from staircasing. We can fix the
staircasing with a `do` block since `lookup` returns a `Maybe` monad.

```haskell
lookup1 :: (Eq a) => a -> [(a, Maybe [b])] -> Maybe [b]
lookup1 k l = case lookup k l of
                Just (Just v@(_:_)) -> Just v
                _ -> Nothing

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
  title  <- lookup1 "title" alist
  user   <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return $ MovieReview title user review
```

We can go even further by using a lifting function.

```haskell
liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist =
  liftM3 MovieReview (lookup1 "title" alist)
                     (lookup1 "user" alist)
                     (lookup1 "review" alist)
```

While the `liftM` functions solve our problem, they're only defined up to
`liftM5`. We can generalize to any number of parameters using the `ap`
function.

```haskell
apReview :: [(String, Maybe String)] -> Maybe MovieReview
apReview alist =
  MovieReview `liftM` lookup1 "title" alist
                 `ap` lookup1 "user" alist
                 `ap` lookup1 "review" alist
```

The `MovieReview` constructor takes 3 arguments but `liftM` first argument is a
function  that only takes one argument. For that reason the result of the first
line is a monad that holds a partial function.

```haskell
ghci> :t  MovieReview `liftM` lookup1 "title" []
MovieReview `liftM` lookup1 "title" []
  :: Maybe (String -> String -> MovieReview)
```

That means the title has been passed to `MovieReview` but that user and review
have not. That's were the `ap` function comes in.

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
```

The `ap` function promotes function application. It does almost the same as
`liftM` but assumes the function is stored in a monad.

## Monad Plus

Here is a representation of a person's phone numbers:

```haskell
type Phone   = String
data Context = Home | Mobile | Business
               deriving (Eq, Show)

joe = [ (Home     , "+35-652-55-512") ]
ivy = [ (Mobile   , "+47-922-55-512")
      , (Business , "+47-922-12-121")
      , (Home     , "+47-925-55-131")
      , (Business , "+47-922-25-551")
      ]
sam = [ (Business , "+26-02-55-5121") ]
```

If we want to get a personal number, starting with home, we can do a simple
search.

```haskell
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                      Nothing -> lookup Mobile ps
                      Just n -> Just n
```

If want to get all business numbers, or mobile numbers if no business numbers
exist, we have to return a list.

```
contextIs :: Context -> (Context, Phone) -> Bool
contextIs a (b, _) = a == b

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter (contextIs Business) ps of
                  [] -> filter (contextIs Mobile) ps
                  ns -> ns
```

In both functions the case structure is similar. The `MonadPlus` type class let
us abstract this pattern.

```haskell
class Monad m => MonadPlus m where
   mzero :: m a 
   mplus :: m a -> m a -> m a
```

The `MonadPlus` adds support for choice and failure. The choice and failure are
defined by `mplus` and `mzero`, respectively. Let's see some examples.

```haskell
instance MonadPlus Maybe where
   mzero = Nothing
   Nothing `mplus` ys = ys
   xs      `mplus` _  = xs
```

For `Maybe` failure is defined as `Nothing` and choice as the first successful
option.

```haskell
ghci> Just 'a' `mplus` Just 'b'
Just 'a'
ghci> Just 'a' `mplus` Nothing
Just 'a'
ghci> Nothing `mplus` Just 'b'
Just 'b'
gchi> Nothing `mplus` Nothing
Nothing
```

This is just one of many options for defining choice and failure.

```haskell
instance MonadPlus [] where
   mzero = []
   mplus = (++)
```

For list failure is defined as an empty list and choice as appending the
options.

```
ghci> "a" `mplus` "b"
"ab"
ghci> "a" `mplus` []
"a"
ghci> [] `mplus` "b"
"b"
ghci> [] `mplus` []
[]
```

We can use `mplus` to replace our `case` expressions.

```bash
oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
```
