module Chap3
import Data.Vect

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = insert x $ insSort xs
-- n and elem are bound here, so we should use a new name
  where insert : Ord e => e -> Vect k e -> Vect (S k) e
        insert x [] = [x]
        insert x (y :: xs) = case x < y of
                                  True => x :: y :: xs
                                  False => y :: insert x xs
        
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = my_length xs + 1

-- explicit bound is needed
my_vec_length : {n: _} -> Vect n a -> Nat
my_vec_length xs = n

my_reverse : List a -> List a
my_reverse xs = rev_acc [] xs
  where rev_acc : List a -> List a -> List a
        rev_acc acc [] = acc
        rev_acc acc (x :: xs) = rev_acc (x :: acc) xs

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vec_map : (a -> b) -> Vect n a -> Vect n b
my_vec_map f [] = []
my_vec_map f (x :: xs) = f x :: my_vec_map f xs


transposeMat : {n: _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate n []
transposeMat (x :: xs) = zipWith (::) x $ transposeMat xs

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith (\x, y => zipWith (+) x y) xs ys


multHelper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multHelper [] ys = []
multHelper (x :: xs) ys = map (\y => sum $ zipWith (*) x y) ys :: multHelper xs ys

multMatrix : Num a => {p: _} -> Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multHelper xs $ transposeMat ys


