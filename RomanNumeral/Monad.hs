module RomanNumeral.Monad where 

import Control.Applicative hiding (optional)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Maybe
import Data.String

newtype Parser a = Parser (String -> [(a, String)])
parse (Parser p) = p

instance Functor Parser where
  fmap  = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap 

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
  return v = Parser (\inp -> [(v, inp)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
  mzero       = Parser (\inp -> [])
  p `mplus` q = Parser (\inp -> (parse p inp ++ parse q inp))

item :: Parser Char
item = Parser (\cs -> case cs of
  ""     -> []
  (c:cs) -> [(c, cs)])

--probably not needed
sat   :: (Char -> Bool) -> Parser Char
sat p = do { c <- item; if p c then return c else mzero}

--probably not needed
char   :: Char -> Parser Char
char c = sat (c ==)

(|.)   :: Parser a -> Parser a -> Parser a
p |. q = Parser (\cs -> case parse (p <|> q) cs of
                            []     -> []
                            (x:xs) -> [x])

upTo     :: Int -> Parser a -> Parser [a]
upTo 0 _ = return []
upTo n p = exactly n p |. upTo (n-1) p |. return []
  where exactly 0 _ = return []
        exactly n p = do { a <- p; as <- exactly (n-1) p; return (a:as)}

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


