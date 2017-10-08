module Movie where

import Control.Monad (liftM, liftM3, ap)

data MovieReview = MovieReview
  { revTitle :: String
  , revUser :: String
  , revReview :: String
  } deriving (Show)

-- | Returns a movie review if all elements exist in the
-- associative list and the values are not empty
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

liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist =
  liftM3
    MovieReview
    (lookup1 "title" alist)
    (lookup1 "user" alist)
    (lookup1 "review" alist)

apReview :: [(String, Maybe String)] -> Maybe MovieReview
apReview alist =
  MovieReview `liftM` lookup1 "title" alist
                 `ap` lookup1 "user" alist
                 `ap` lookup1 "review" alist
