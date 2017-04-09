module Main

import Tree 
import Picture

total listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

total treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++
                                   val :: (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Subtract Expr Expr
          | Mult Expr Expr

total evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (+) (evaluate x) (evaluate y)
evaluate (Subtract x y) = (-) (evaluate x) (evaluate y)
evaluate (Mult x y) = (*) (evaluate x) (evaluate y)

total maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case compare x y of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x

total biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive orig@(Triangle x y)) = Just (area orig)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
