module VCard where

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String
