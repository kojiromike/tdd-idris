import Data.Vect

addRow : Num a => Vect n a -> Vect n a -> Vect n a
addRow [] [] = []
addRow (x :: xs) (y :: ys) = x + y :: addRow xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addRow x y :: addMatrix xs ys
