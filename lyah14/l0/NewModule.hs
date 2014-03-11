module NewModule where

-- :l name - load
-- :t get type

pie = 3.14

-- This is a comment

{- This is
 - a multiline comment -}

-- answer = 42 :: Int can do this
-- but more common to do this
answer :: Int
answer = 42

s :: String
s = "foobar"
b, c, d :: Bool
b = True
c = False
d = b || d

list = [1, 2, 3]

atuple :: (Double, String)
atuple = (1, "foo")
btuple = (2, "bar")

list2 = [atuple, btuple]
list3 = 4 : list 
list4 = list ++ [4]

sinpi = sq (sin pie)

sq x = x * x

ssin x = sin x

z = ssin pi

zz = map sq list
