import Data.Vect

total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = Vect.replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

total createEmpties : Num a => (x : Vect 0 a) ->
                               (xs : Vect len (Vect 0 a)) ->
                               Vect (S len) (Vect p a)
createEmpties [] xs = ?createEmpties_rhs_1

total multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] [] = []
multMatrix [] _ = []
multMatrix (x :: xs) [] = createEmpties x xs
multMatrix (x :: xs) (y :: ys) = ?multMatrix_rhs_2
