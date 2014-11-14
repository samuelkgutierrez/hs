## Haskell Notes for a Compete Beginner -- Me

Most of the content from these notes comes from: http://learnyouahaskell.com

- Compilers
The Haskell Platform
ghc
ghci <== interactive shell
Common Extension .hs

- Load Haskell
```
:l <foo>
```

Haskell has a static type system.

Functions can't begin with uppercase letters.

Lists are a homogeneous data structure.

++ List Concatenation

[1, 2, 3] ++ [4]
    ^
    |
   Slow to add at end of large lists

: 'cons' to add to front of list is fast

If you want to get an element out of a list by index, use !!. The indices start
at 0.

```
*Main> let foo = [0..12]
*Main> foo
[0,1,2,3,4,5,6,7,8,9,10,11,12]
```

```
-- Single line comment

:t Expr Type introspection

:: Read: "has type of"
```

Int Type: More efficient. Integer can represent large numbers

```
ghci> :t head
head :: [a] -> a
```
'a' here is a 'type variable'. That is, 'a' can be any type.

Everything before the => symbol is called a class constraint

"fromIntegral :: (Num b, Integral a) => a -> b" is a type declaration

There's also a thing called as patterns. Those are a handy way of breaking
something up according to a pattern and binding it to names while still
keeping a reference to the whole thing. You do that by putting a name and an @
in front of a pattern. For instance, the pattern xs@(x:y:ys). This pattern will
match exactly the same thing as x:y:ys but you can easily get the whole list
via xs instead of repeating yourself by typing out x:y:ys in the function body
again. Here's a quick and dirty example:

```
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

### Tue Mar  4 11:03:30 MST 2014 (Haskell Class Day 0)
Mailing List: lyah14
Named after Haskell Curry, American logician
Haskell 1.0: 1990
Current major implementation: Glasgow Haskell Compiler
    - Written in Haskell, some C runtime stuff...

Pure by default: no side effects
    - no destructive updates
    - see referential transparency

Lazy - technically, it's not strict

Awesome type system

Read: Monads are like burritos...

```
:i Type gives awesome info
:t (:)
```

See: Bryan O'Sullivan's code for great examples.

(:) 1 []

Function composition: (.) :: (b -> c) -> (a -> b) -> (a -> c) ~ f(g(x))

!  Tue Mar 25 11:03:40 MDT 2014

```
:t id (Identity function) a -> a
```

Hindley Milner type system

@see Planet Haskell

NICE for Hacking: Use undefined. Like: ontable = undefined

Type variables: lower-case variables in: map :: (a -> b) -> [a] -> [b]

@see Recursive data definition.

Parametric polymorphism ~ C++ templates: really no overhead.

SPECIALIZE pragma

* -> *

Folds and zip on trees! For awesome tree stuff, look at the standard library.

@see Haskell "Maybe" Used for computations that could fail.
@see Haskell "Just"

-- Tue Apr  8 11:05:24 MDT 2014
@ - as expression to name things
$ - associativity
deriving (Show, Eq, Read) ~ 7 classes supported out of the box
pragma {#- LANGUAGE GeneralizedNewtypeDeriving #-}
@see functor pattern in OOP
dropping formal arguments is idiomatic

```
ys' = map (2*) xs <-- do it this way
```

```
-- flip can bail you out if you're stuck with the wrong arg order
flip
```

foldl' = strict accumulator - use this one

` operator deals with infix stuff

### Tue Apr 15 11:23:53 MDT 2014
zipWith (\x y -> x * y) xs ys
BETTER!
zipWith (*) xs ys
zipWith (\x y -> if x > y then x else y) xs ys

Know: Pointfree Form

Think: when writing functions, make it as easy as possible for users to curry
the functions. Often, this means that placing the least-changing argument first
(from the left).

Free to make your own infix operators.

Type constructors

```
:i - info
:kind
```

```
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

Know about type class Functor

```
[] Int -> [] Int aka [Int] -> [Int]
```

```
ghci:
:m + <mod name> adds module
:m - <mod name> rms module
```

### Tue Apr 22 11:04:43 MDT 2014

```
data MyList = ANode a (MyList a)
            | Nil

data Tree a = Node a (Tree a) (Tree a) | Empty
```

-- a is a type variable

```
:kind
:type
:info Num
```

Language Pragmas: cmdline pass with -X

Learn about Lifting

```
:t it -- last thing
```

Something about Haskell can give you the ASTs and stuff. Talk to Tim.

() - "Unit" - Only interesting because of its side effects

>>= - "Bind"

unsafePerformIO

### Tue Apr 29 11:10:37 MDT 2014
Explicit importation
```
import FizzBuzz (fizzbuzz)

import qualified FizzBuzz as FB
```

readFile

Monads must have:
return: a -> m a
bind: m a -> (a m b) -> m b
fail:

know "do" notation

-- Creating Project

### Tue May 27 11:58:32 MDT 2014
Let's Parse some Stuff with Attoparsec
Example in RWH Chapter 16

type Parser = Parser Text

? -XOverloadedStrings

<$> : Lives in Control.Applicative : 'fMap'

-- Tue Jun 24 11:06:34 MDT 2014
.ghci and module search paths

```
{-# LANGUAGE BangPatterns #-}
```
