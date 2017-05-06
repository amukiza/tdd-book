module InserSort

import Data.Vect

insert : Ord a => (x : a) -> (xs : Vect k a) -> Vect (S k) a
insert x [] = [x]
insert x (y :: xs) = if x < y
                        then x :: y :: xs
                        else y :: insert x xs

insertSort : Ord a => Vect n a -> Vect n a
insertSort [] = []
insertSort (x :: xs) = let sortedXs = insertSort xs in
                       insert x sortedXs

mlength: List a -> Nat
mlength [] = 0
mlength (x :: xs) = 1 + mlength xs

mReverse : List a -> List a
mReverse [] = []
mReverse (x :: xs) = mReverse xs ++ [x]

mMap : (a -> b) -> List a -> List b
mMap f [] = []
mMap f (x :: xs) = f x :: mMap f xs

mVectMap :(a -> b) -> Vect n a -> Vect n b
mVectMap f [] = []
mVectMap f (x :: xs) = f x :: mVectMap f xs
