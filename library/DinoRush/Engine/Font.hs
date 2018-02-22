module DinoRush.Engine.Font where

data Number
  = Number'0
  | Number'1
  | Number'2
  | Number'3
  | Number'4
  | Number'5
  | Number'6
  | Number'7
  | Number'8
  | Number'9
  deriving (Show, Eq, Enum, Bounded)

numberToInt :: Number -> Int
numberToInt = fromEnum

toNumber :: Integer -> [Number]
toNumber i = (if next == 0 then [] else toNumber next) ++ (single (i `mod` 10))
  where
    next = i `div` 10
    single 0 = [Number'0]
    single 1 = [Number'1]
    single 2 = [Number'2]
    single 3 = [Number'3]
    single 4 = [Number'4]
    single 5 = [Number'5]
    single 6 = [Number'6]
    single 7 = [Number'7]
    single 8 = [Number'8]
    single 9 = [Number'9]
    single _ = []

toNumberReverse :: Integer -> [Number]
toNumberReverse = reverse . toNumber
