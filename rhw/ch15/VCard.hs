module VCard where

import Control.Monad (mplus)

data Context
  = Home
  | Mobile
  | Business
  deriving (Eq, Show)

type Phone = String

joe :: [(Context, Phone)]
joe = [(Home, "+355-652-55512")]

ivy :: [(Context, Phone)]
ivy =
  [ (Mobile, "+47-922-55-512")
  , (Business, "+47-922-12-121")
  , (Home, "+47-925-55-131")
  , (Business, "+47-922-25-551")
  ]

sam :: [(Context, Phone)]
sam = [(Business, "+260-02-55-5121")]

contextIs :: Context -> (Context, Phone) -> Bool
contextIs a (b, _) = a == b

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                      Nothing -> lookup Mobile ps
                      Just n -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter (contextIs Business) ps of
                  [] -> filter (contextIs Mobile) ps
                  ns -> ns

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
