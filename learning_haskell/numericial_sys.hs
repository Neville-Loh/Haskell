import Data.List
import System.IO

-- Int -2^65


main = putStrLn "yo"

toNat :: Int -> Nat
toNat 0 = Zero
toNat m = S (toNat (m - 1))

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (S Zero) = 1
fromNat (S n) = 1 + fromNat(n)

data Nat = Zero | S Nat
  deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (S n) m = S (add n m)

mul :: Nat -> Nat -> Nat
mul n Zero = Zero
mul n (S m) = add n (mul n m)

subt :: Nat -> Nat -> Nat
subt n Zero = n
subt Zero n = Zero
subt (S n ) (S m) = subt n m

divo :: Nat -> Nat -> Nat 
divo n Zero = Zero
divo m n =
  if (greaterthan m n)
    then add (divo (subt m n) n) (S Zero)
  else Zero

eq :: Nat -> Nat -> Bool
-- why does it not work???
eq m n = (lessthaneq m n) && (lessthaneq n m )

lessthaneq :: Nat -> Nat -> Bool
-- m <= n
lessthaneq Zero n = True
lessthaneq  m Zero = False 
lessthaneq (S m) (n)  = lessthaneq m n

greaterthaneq :: Nat -> Nat -> Bool
greaterthaneq n Zero = True
greaterthaneq Zero n = False
greaterthaneq (S m) (n) = greaterthaneq m n




greaterthan :: Nat -> Nat -> Bool
greaterthan m n = not ( lessthaneq m n )

