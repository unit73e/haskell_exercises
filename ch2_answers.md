# Types and functions answers

## 1

Haskell is strongly typed because it does not allow to use a type as if it
were another type. Unlike other languages, such as C or Java, Haskell does
not have automatic conversion from value of one type to another type.

## 2

Haskell is statically typed because the type of every value and expression
is known at compilation, before the program is executed. The advantage is
that common errors are caught before the program is executed.

## 3

Type inference means that the compiler can automatically deduce the types
of expressions, without the programmer having to explicitly specify the
types in the source code. Haskell can deduce the types of almost all
expressions.

## 4

Haskell type system is safe. The main advantage of a strong and static
type system over a weak and dynamic type system is that types are checked
at compilation time instead of runtime. Any trivial error is caught early.
Another advantage is that refactoring is easier and safer.

Haskell type system is stronger than languages such as C or Java. Haskell
does not allow casting or automatic conversion because often these are a
source of many errors.  The disadvantage is that sometimes when you could
simply convert a value to another type, you have to copy the value
instead.

Haskell has type inference. Unlike some languages, such as C# or Java,
types do not have to be explicitly specified. This allows to have
expressiveness as powerful as dynamic languages while maintaining the
benefits of statically typed languages.

## 5

Add `:: MyType` to the value. For example:

```haskell
Prelude> 'a' :: Char
'a'
```

## 6

Write the function followed by the arguments separated by spaces. For
example:

```haskell
Prelude> odd 2
False
Prelude> even 2
True
Prelude> compare 2 4
LT
Prelude> compare 2 2
EQ
Prelude> compare 2 1
GT
```

Additionally functions have higher precedence than any infix operator, so
both expressions are equivalent:

```haskell
Prelude> (compare 1 2) == LT
True
Prelude> compare 1 2 == LT
True
```

Arguments can be wrapped in parentheses, but add visual noise. However
sometimes parentheses are required:

```haskell
Prelude> compare (sqrt 3) (sqrt 4)
LT
```

## 7

The easiest way is to use the `head` function. For example:

```haskell
Prelude> head [1,2,3,4]
1
:```

As the name of the function suggests, in functional languages, the first
element of a list is called the head of the list.

## 8

The quickest way is to use the `tail` function. For example:

```haskell
Prelude> tail [1,2,3,4]
[2,3,4]
```

While the first element of a list is called the head of the list, in
functional languages, the remaining elements are called the tail of the
list.

## 9

Because the values of a list can be of any type.

## 10

Add `:: [MyType]` after the list. For example:

```haskell
Prelude> ['a','b','c'] :: [Char]
"abc"
```

## 11

We write a tuple by enclosing the elements in parentheses and separating
them with commas. For example:

```haskell
Prelude> ('a',1)
('a',1)
```

## 12

Lists have variable size and must have all elements of the same type while
tuples have fixed size and can have values of different types.

## 13

Two tuples have the same type if both have the same size and the type of
the values is the same in the same order. For example:

```haskell
Prelude> let a = ('a', True)
Prelude> let b = ('b', False)
Prelude> let c = (True, 'a')
Prelude> :type a
a :: (Char, Bool)
Prelude> :type b
b :: (Char, Bool)
Prelude> :type c
c :: (Bool, Char)
```

The tuples 'a' and 'b' have the same type, '(Char, Bool)'. The tuple 'c'
does not have the same type as 'a' and 'b' but '(Bool, Char)' instead.

## 14

The unit type is a special type written `()` that acts as an empty tuple.
This type only has one value also written `()`. 

## 15

You don't because Haskell does not have the notion of one element tuple.

## 16

We use the `take` function. For example:

```haskell
Prelude> take 2 [1,2,3,4]
[1,2]
```

## 17

We use the `drop` function. For example:

```haskell
Prelude> drop 2 [1,2,3,4]
[3,4]
```

## 18

Use the `fst` function. For example:

```haskell
Prelude> fst (1,2)
1
```

## 19

Use the `snd` function. For example:

```haskell
Prelude> snd (1,2)
2
```

## 20

Use the `lines` function. For example:

```haskell
Prelude> lines "the quick\nbrown fox\njumps"
["the quick","brown fox","jumps"]

## 21

We can get the type of `lines` using the `:type` command:

```haskell
Prelude> :type lines
lines :: String -> [String]
```

The function `lines` has type `String` to list of `String`.

## 22

A function has side effects if the result of the function has type `IO`.
For example:

```haskell
Prelude> :type readFile
readFile :: FilePath -> IO String
```

By default functions in Haskell do not have side effects.

## 23

A pure function.

## 24

An impure function.

## 25

We define the function in the `add.hs` file:

```haskell
-- file add.hs
add :: Num a => a -> a -> a
add a b = a + b
```

The function can be tested in GHCI as follows:

```haskell
Prelude> :load add.hs 
[1 of 1] Compiling Main             ( add.hs, interpreted )
Ok, modules loaded: Main.
*Main> add 1 2
3
```

## 26

We can test this with the following file:

```haskell
-- file var.sh
x=1
x=2
```

If we load the file in GHCI:

```haskell
Prelude> :load var.hs 
[1 of 1] Compiling Main             ( var.hs, interpreted )

var.hs:2:1:
    Multiple declarations of ‘x’
    Declared at: var.hs:1:1
                 var.hs:2:1
Failed, modules loaded: none.
```

The compiler complains there are multiple declarations of 'x'. This is
because in Haskell variables are immutable.

## 27

A simple expression is easy

```haskell
Prelude> if True then 1 else 0
1
```

## 28

It will output an error because in Haskell `if` is an expression, not a
statement. An expression evaluates to a value while a statement do not.

## 29

First lets test the original implementation:

```haskell
Prelude> drop 2 "test"
"st"
Prelude> drop 4 [1,2]
[]
Prelude> drop 0 [1,2]
[1,2]
Prelude> drop 2 []
[]
Prelude> drop (-2) [1,2]
[1,2]
```

One possible implementation is:

```haskell
-- file myDrop.hs
myDrop :: Int -> [a] -> [a]
myDrop n xs = 
    if n <= 0 || null xs
        then xs
        else myDrop (n - 1) (tail xs)
```

Now we check if out implementation matches the `drop` function:

```haskell
Prelude> :load myDrop.hs 
[1 of 1] Compiling Main             ( myDrop.hs, interpreted )
Ok, modules loaded: Main.
*Main> drop 2 "test"
"st"
*Main> drop 4 [1,2]
[]
*Main> drop 0 [1,2]
[1,2]
*Main> drop 2 []
[]
*Main> drop (-2) [1,2]
[1,2]
```

## 30

Because in haskell functions are by default non strictly evalutated (or
lazy evaluated).

It is easy to define a logical or that short circuits:

```haskell
-- file myOr.hs
myOr :: Bool -> Bool -> Bool
myOr a b = if a then a else b
```

Non strict evaluation means that expressions are not evaluated until
required. If `a` is `True` then `b` not be evaluated.

## 31

-- TODO

## 32

-- TODO

## 33

-- TODO

## 34

-- TODO

## 35

Subtype polymorphism allows a value to be transformed into another if it
is a subtype of the later. This type of polymorphism is a characteristic
of object oriented languages. Haskell is not object oriented and for that
reason it does not provide subtype polymorphism.

## 36

Coercion polymorphism allows a value of one type to be implicitly
converted into another type. One example is automatic convertion between
integers and floating points. Haskell avoids coercion polymorphism to
guarantee predictability.

## 37

Because in Haskell a function always always has one parameter.

-- TODO

## 38

The main advantage of pure functions is predictability and readability.
The name and the type signature of a pure function alone already gives us
an idea of what it does. For example:

```haskell
Prelude> :type not
not :: Bool -> Bool
```

Just by checking the type we already know the function will return either
`True` or `False`. By checking the name of the function we know it will
negate its argument. If the function was impure we could not have this
guarantee because at some point the 
