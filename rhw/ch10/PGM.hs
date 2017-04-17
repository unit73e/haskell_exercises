module PGM(Graymap(..)) where

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
