myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ []         = Nothing
myLookup e ((k,v):xs) = if e == k then Just v else myLookup e xs
