module Main where

main :: IO ()
main = putStrLn "hi"

drop' :: Int -> [a] -> [a]
drop' n xs = if n <= 0 || null xs
             then xs
             else drop' (n - 1) (tail xs)

isOdd :: Int -> Bool
isOdd n = mod n 2 == 1

or' :: Bool -> Bool -> Bool
or' a b = if a
          then a
          else b

lastButOne :: [a] -> a
lastButOne xs = if null xs || length xs == 1
                then error "List must have > 1 item"
                else xs !! max 0 (length xs - 2)

type BookID = Int

type BookTitle = String

type AuthorList = [String]

data Book = Book BookID BookTitle AuthorList
  deriving (Show)

data Magazine = Magazine Int String [String]
  deriving (Show)

type CardNumber = String

type CardHolder = String

type Address = [String]

type CustomerID = Int

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
  deriving (Show)

data RainbowColors = Red
                   | Orange
                   | Yellow
                   | Green
                   | Blue
                   | Indigo
                   | Violet
  deriving (Eq, Show)

not' :: Bool -> Bool
not' True = False
not' False = True
