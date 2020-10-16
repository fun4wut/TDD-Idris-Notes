module Hello

export
fib : Nat -> Nat
fib n = fibAcc n 0 1 where
  fibAcc : Nat -> Nat -> Nat -> Nat
  fibAcc Z pp p = p
  fibAcc (S n) pp p = fibAcc n p (pp+p)

reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs

partial getType : Char -> Type
getType 'i' = Int
getType 'r' = Double
getType 'b' = Bool


