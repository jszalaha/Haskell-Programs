--------------------------------------------------
-- hw5.hs
-- author: Jordan Zalaha
-- p.p. parter: Shrey Valia
-- email: jszalaha@ucsc.edu
-- date: March 11, 2016
-------------------------------------------------

import Data.Char
import System.IO
import Control.Monad
import Control.Applicative (Applicative,pure,(<*>))

-- Parser Framework

data Parser a = Parser (String -> [(a,String)])

run :: Parser a -> String -> [(a,String)]
run (Parser f) s = f s

satP :: (Char -> Bool) -> Parser Char
satP pred = Parser (\cs -> case cs of
                            []  -> []
                            c:cs' -> if pred c then [(c,cs')] else [])

digit = satP isDigit

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  pa >>= fpb = Parser (\cs -> do (a,cs') <- run pa cs
                                 run (fpb a) cs')

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\cs -> run p2 cs ++ run p1 cs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = return [] <|> oneOrMore p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p
                 xs <- zeroOrMore p
                 return (x:xs)

first :: Parser a -> Parser a
first p = Parser (\cs -> case run p cs of
                          [] -> []
                          (r:rs) -> [r])

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p opp = do x <- p
                 tryMore x
  where tryMore x = first (return x <|> more x)
        more x = do op <- opp
                    y <- p
                    tryMore (op x y)

intP :: Parser Int
intP = do digits <- first (oneOrMore digit)
          return (read digits)

-- isDot predicate to check that input is '.'
isDot :: Char -> Bool
isDot c = if c == '.'
	  then True
	  else False

-- dot parser
dot = satP isDot

-- double parser
doubleP :: Parser Double
doubleP = first ( do i <- intP
                     return (read (show i ++ ".0"))
                  <|> do i <- intP
                         dot
                         dec <- intP
                         return (read (show i ++ "." ++ show dec))
                )

addOp :: Parser (Double -> Double -> Double)
addOp = do satP (=='+'); return (+)
	      <|> do satP (=='-'); return (-)

mulOp :: Parser (Double -> Double -> Double)
mulOp = do satP (=='*'); return (*)
	      <|> do satP (=='/'); return (/)

expOp :: Parser (Double -> Double -> Double)
expOp = do satP (=='^'); return (**)

calc :: Parser Double
calc = let mulExpr = chain doubleP mulOp
       in  chain mulExpr addOp

calc2 :: Parser Double
calc2 = let expExpr = chain doubleP expOp
            mulExpr = chain expExpr mulOp
	      in  chain mulExpr addOp