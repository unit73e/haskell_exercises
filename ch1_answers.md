# General exercises

## 1

Glasgow Haskell Compiler

## 2

There are three main components:

 - `ghc` is the compiler
 - `ghci` is the interactive interpreter and debugger
 - `runhaskell` is a program to run haskell scripts, without needing to compile them

## 3

Because the prompt is by default set to `"%s> "` where `%s` is replaced by the
names of the modules currently in scope. Prelude is a module that is always
loaded implicitly in any haskell program so it is always in scope.

## 4

Run the following command:

```haskell
:set prompt <prompt>
```

For example, to set the prompt to `ghci> `:

```haskell
Prelude> :set prompt "ghci> "
ghci> 
```

## 5

Run the following command:

```haskell
:module + <mod>
```

For example, to add the `Data.Ratio` module:

```haskell
Prelude> :module + Data.Ratio 
Prelude Data.Ratio> 
```

The `Data.Ratio` module adds support to rational numbers:

```haskell
Prelude Data.Ratio> 1%4 + 1%4
1 % 2
```

## 6

Enclose the `+` operator between parentheses followed by the operands.

For example:

```haskell
Prelude> (+) 2 2
4
```

## 7

An infix operator and its operands do not have to be separated by spaces:

```haskell
Prelude> 2+2
4
Prelude> 2+ 2
4
Prelude> 2 +2
4
```

What this means is that `2 + -3` is equivalent to `2 + - 3`. For that reason
the interpreter complains two infix operators are being in the same expression.

To fix the expression the negative number must be enclosed in parentheses:

```haskell
2 + (-3)
```

This makes `-` a unary operator and the expression is correctly evaluated.

## 8

An operator may have more than one character. This means the expression `2*-3`
is equivalent to `2 *- 3`. By default the operator `*-` is not defined.

To fix the expression the negative number must be enclosed in parentheses:

```haskell
2+(-3)
```

This treats `+` and `-` as separate operators.

## 9

Execute the following command:

```haskell
:info <name>
```

For example, to know the associativity and precedence of the `+` operator:

```haskell
Prelude> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 6 +
```

The last line indicates that indicates that the `+` operator is a left
associative infix operator with precedence 6. A higher number means higher
precedence.

The `*` operator has higher precedence than the `+` operator:

```haskell
Prelude> :info (*)
class Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 7 *
```

As a result the following operations are equivalent:

```haskell
Prelude> 1 + 4 * 4 + 2
19
Prelude> 1 + (4 * 4) + 2
19
```

The `-` operator has the same associativity and precedence has the `+`
operator:

```haskell
Prelude> :info (-)
class Num a where
  ...
  (-) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 6 -
```

For that reason the following expressions are equivalent:

```haskell
Prelude> 1 + 2 - 3 + 1
1
Prelude> ((1 + 2) - 3) + 1
1
```

The `^` operator has right associatity:

```haskell
Prelude> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^
```

So the following expressions are equivalent:

```haskell
Prelude> 2^1^2^3
2
Prelude> 2^(1^(2^3))
2
```

## 10

To define a constant in `ghci` use the `let` contructor. For example, to define
the `e`:

```haskell
Prelude> let e = exp 1
Prelude> e
2.718281828459045
```

## 11

A list is defined by having data separated by commas and enclosed in brackets.
For example:

```haskell
Prelude> [1,2,3]
[1,2,3]
```

An empty list follows the same rule:

```haskell
Prelude> []
[]
```

## 12

Because lists must have all elements of the same type.

## 13

The easieast and shortest way is to use the enumeration notation:

```haskell
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

## 14

Specify the first two elements and the interpreter will figure out the
remaining elements of the sequence:

```haskell
Prelude> [1.0, 1.25..2.0]
[1.0,1.25,1.5,1.75,2.0]
```

## 15

The first two elements must be specified when defining a sequence in descending
order:

```haskell
Prelude> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]
```

If only the first element is specified the interpreter will assume the sequence
is in ascending order, resulting in an empty list:

```haskell
Prelude> [10..1]
[]
```

## 16

The `++` operator concatenates two lists:

```haskell
Prelude> [1,2,3] ++ [4,5]
[1,2,3,4,5]
```

Note that the cost of this operation is equal to the size of the first list.

## 17

The `:` operator adds a element to the beggining of a list:

```haskell
Prelude> 1 : [2,3]
[1,2,3]
```

The `:` operator is pronounced "cons" which is short for constructor.

## 18

Strings are defined by placing characters between double quotes:

```haskell
Prelude> "My String"
"My String"
```

## 19

Characters are defined with a single character between single quotes:

```haskell
Prelude> 'a'
'a'
```

## 20

String are implemented as a list of characters:

```haskell
Prelude> let a = ['a','b','c']
Prelude> let b = "abc"
Prelude> a == b
True
Prelude> "" == []
True
Prelude> 'a':"bc"
"abc"
Prelude> "ab" ++ "cd"
"abcd"
```

## 21

By executing the command `:type`. For example:

```haskell
Prelude> :type 'a'
'a' :: Char
Prelude> :type "abc"
"abc" :: [Char]
```

## 22

The following program should print the number of lines of a file:

```haskell
-- file: lc.hs
main = interact lineCount
  where lineCount input = show (length (lines input)) ++ "\n"
```

The `runhaskell` command can be used to run the program:

```bash
$ runhaskell lc.hs < lc.hs 
3
```

## 23

```haskell
-- file: wc.hs
main = interact lineCount
  where lineCount input = show (length (words input)) ++ "\n"
```

## 24

```haskell
-- file: wc.hs
main = interact lineCount
  where lineCount input = show (length input) ++ "\n"
```
