# Chapter 15

Let's use an association list to fill the following data structure:

```haskell
data MovieReview = MovieReview
  { revTitle :: String
  , revUser :: String
  , revReview :: String
  } deriving (Show)
```

We'll start with the most obvious solution:

```
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

A `MovieReview` is returned if the title, user and review keys exist and the
values are not empty. Otherwise `Nothing` is returned. Let's do some testing:

```haskell
-- Returns a MovieReview since every key and value is present
ghci> simpleReview [ ("title", Just "Whatership Down")
                   , ("user", Just "Richard Adams")
	           , ("review", Just "Excelent")
	           ]
Just (MovieReview { revTitle = "Whatership Down"
                  , revUser = "Richard Adams"
                  , revReview = "Excelent"
                  })

-- Returns Nothing since review's value is empty
ghci> simpleReview [ ("title", Just "Whatership Down")
                   , ("user", Just "Richard Adams")
	           , ("review", Just "")
	           ] 
Nothing

-- Returns Nothing since review is missing
ghci> simpleReview [ ("title", Just "Whatership Down")
                   , ("user", Just "Richard Adams")
	           ] 
Nothing

-- Returns Nothing since review value is Nothing
ghci> simpleReview [ ("title", Just "Whatership Down")
                   , ("user", Just "Richard Adams")
		   , ("review", Nothing)
	           ] 
Nothing
```

This version works but suffers from staircasing. We can fix the staircasing
with a `do` block because `lookup` returns a `Maybe` monad.

```haskell
lookup1 :: (Eq a) => a -> [(a, Maybe [b])] -> Maybe [b]
lookup1 k l =
  case lookup k l of
    Just (Just v@(_:_)) -> Just v
    _ -> Nothing

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
  title <- lookup1 "title" alist
  user <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return $ MovieReview title user review
```

We can go even further by using a lifting function.

```haskell
liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist =
  liftM3
    MovieReview
    (lookup1 "title" alist)
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

Remember that the `liftM` promotes a function to a monad.

```haskell
liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1 = do { x1 <- m1; return (f x1) }
```

In other words `liftM` executes a given function over the value of a given
monad and injects the result into a monad of the same type. A simple example
should clarify this.

```haskell
ghci> (+1) `liftM` Just 1
Just 2
```

The `(+1)` function is executed over `1` and the result is injected into
`Just`. In `apReview` case `MovieReview` is the function and the monad is
`Maybe` since that's what `lookup1` returns.

```
ghci> :t MovieReview
MovieReview :: String -> String -> String -> MovieReview

ghci> :type MovieReview `liftM` lookup1 "title" []
MovieReview `liftM` lookup1 "title" []
  :: Maybe (String -> String -> MovieReview)
```

Since `MovieReview` is only executed over one argument the result will be a
partial function, which will be injected into a `Maybe` monad. Namely title has
been passed to `MovieReview` but user and review have not. That's were the `ap`
function comes in.
