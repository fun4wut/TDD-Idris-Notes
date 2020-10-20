module Chap4
import Data.Vect

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => BSTree elem -> elem -> BSTree elem -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case compare x y of
                                         LT => Node (insert x left) y right
                                         EQ => orig
                                         GT => Node left y (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

treeToList : BSTree a -> List a
treeToList Empty = [] 
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just $ max x y

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = case x of
                                     (Triangle y z) => Just $ y * z / 2.0
                                     _ => Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate _ x) = biggestTriangle x
biggestTriangle (Translate _ _ x) = biggestTriangle x

vectTake : (m: Fin $ S n) -> Vect n a -> Vect (cast m) a -- cast Fin to Nat
vectTake FZ xs = []
vectTake (FS k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => {n :_} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just i) => Just (Vect.index i xs + Vect.index i ys)



