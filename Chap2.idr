module Chap2
import Data.Strings
import Data.List
import System.REPL

palindrome : String -> Bool
palindrome x = (== x) $ reverse x -- use $ to avoid parentheses

palindrome2 : String -> Bool
palindrome2 x = palindrome $ toLower x

palindrome3 : String -> Bool
palindrome3 x = if length x > 10 then palindrome x else False

palindrome4 : Nat -> String -> Bool
palindrome4 n x = if length x > n then palindrome x else False

counts : String -> (Nat, Nat)
counts x = let wordCnt = length $ words x
               charCnt = length x in
               (wordCnt, charCnt)

top_ten : Ord a => List a -> List a
top_ten x = take 10 $ reverse $ sort x

over_length : Nat -> List String -> Nat
over_length n x = length $ filter ((> n) . length) x -- use . to compose func

main : IO ()
main = repl "Enter a string: " $ (++ "\n") . show . palindrome2

