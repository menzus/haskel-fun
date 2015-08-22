module RomanNumeral.Monad where 

import Control.Applicative hiding (optional)
import RomanNumeral.ParserMonad

letter     :: Char -> Int -> Parser Int
letter c i = do { x <- char c; return i }

i = letter 'I' 1
v = letter 'V' 5
x = letter 'X' 10
l = letter 'L' 50
c = letter 'C' 100
d = letter 'D' 500
m = letter 'M' 1000

minus   :: Parser Int -> Parser Int
minus p = do { a <- p; return (-a) }

(.>)     :: Parser Int -> Parser Int -> Parser Int
p .> q = do {
    a <- p;
    b <- q;
    return (a + b);
  }

optional :: Parser Int -> Parser Int
optional p = do { a <- p; return a } |. return 0

upTo3 :: Parser Int -> Parser Int
upTo3 p = sumP $ upTo 3 p
  where sumP   :: Parser [Int] -> Parser Int
        sumP p = do { as <- p ; return (sum as)}

romanNumeralGroup       :: Parser Int -> Parser Int -> Parser Int -> Parser Int
romanNumeralGroup i v x = fourOrNine <|> fiveAndOnes
  where fourOrNine = minus i .> ( v |. x )
        fiveAndOnes = optional v .> upTo3 i

ones = romanNumeralGroup i v x
tens = romanNumeralGroup x l c
hundreds = romanNumeralGroup c d m

romanNumeral :: Parser Int
romanNumeral = upTo3 m .> optional hundreds .> optional tens .> optional ones 


