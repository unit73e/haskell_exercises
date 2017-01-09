# Key Value Data Structures

## Association Lists

An association list is a list containing `(key, value)` tuples.

```
ghci> l = [(1, "one"), (2, "two"), (3, "three")]
ghci> lookup 2 l
Just "two"
ghci> lookup 5 l
Nothing
```

Association lists are simple but perform poorly with large data sets.

## Maps

A map behaves like an association list but it is ordered and provides much
better performance. The standard library map is implemented as a balanced
binary tree.

The easiest way to build map is to build it from an association list.

```
gchi> Map.fromList [(1,'a'),(2,'b'),(3,'b')]
fromList [(1,'a'),(2,'b'),(3,'b')]
```

You can also build a map using the insert function.

```
ghci> Map.insert 1 'a' . Map.insert 3 'c' . Map.insert 2 'b' $ Map.empty 
fromList [(1,'a'),(2,'b'),(3,'c')]

ghci> foldl (\map (k,v) -> Map.insert k v map) Map.empty [(1,'a'),(2,'b'),(3,'c')]
fromList [(1,'a'),(2,'b'),(3,'c')]
```

## Functions


