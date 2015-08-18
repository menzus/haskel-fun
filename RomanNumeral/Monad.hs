module RomanNumeral.Monad where 

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Char
import Data.Maybe
import Data.String

newtype Parser a = Parser(String -> [(a, String)])
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

--not needed, it's defined in alternative; there it's returning a long list, here it's returning only the first element (because of +++)
mymany   :: Parser a -> Parser [a]
mymany p = mymany1 p |. return []

mymany1   :: Parser a -> Parser [a]
mymany1 p = do {
    a <- p; 
    as <- mymany p;
    return (a:as)
  }

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

zeroOrOne = upTo 1

--not needed 
anyletter = i |. v |. x |. l |. c |. d |. m

--not needed 
anyletters = mymany anyletter

