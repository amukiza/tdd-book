module Matrix

import Data.Vect

createEmpties : Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n a) -> Vect  n (Vect m a)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let transposedXs = transposeMat xs in
      (transposeHelper x transposedXs)
  where
     transposeHelper : (x : Vect n a) -> (xss : Vect n (Vect len a)) -> Vect n (Vect (S len) a)
     transposeHelper [] [] = []
     transposeHelper (x :: xs) (y :: yss) = (x :: y) :: transposeHelper xs yss

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) =
    let addedMat = add x y in
       addedMat :: addMatrix xs ys
    where
       add : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
       add [] [] = []
       add (x :: xs) (y :: ys) = x + y :: add xs ys
