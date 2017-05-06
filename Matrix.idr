module Matrix

import Data.Vect

transposeMat : Vect m (Vect n a) -> Vect  n (Vect m a)
transposeMat [] = replicate _ []
transposeMat (x :: xs) =
    let transposed = transposeMat xs
    in (zipWith (::) x transposed)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) =
    let addedMat = add x y in
       addedMat :: addMatrix xs ys
    where
       add : Num a => Vect m a -> Vect m a -> Vect m a
       add [] [] = []
       add (x :: xs) (y :: ys) = x + y :: add xs ys
