module Main

import Tree 

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

