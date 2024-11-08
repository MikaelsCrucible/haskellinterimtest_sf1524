module Parser where

import Data.List
import Data.Maybe
import Data.Char
import Prelude

import Types
import Examples

-------------------------------------------------------------------

-- The following are given...

ops :: [Operator]
ops = ['+', '-', '*', '/', '^', '(', ')', '$']

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), (')',(1,N)), ('+',(6,L)),
           ('-',(6,L)), ('*',(7,L)), ('/',(7,L)), ('^',(8,R))]

stringToInt :: String -> Int
stringToInt = read

prettyExpr :: Expr -> String
prettyExpr (ENum n) = show n
prettyExpr (EVar s) = s
prettyExpr (EApp op e e') = "(" ++ prettyExpr e ++ [op] ++ prettyExpr e' ++ ")"

-------------------------------------------------------------------

precedence :: Operator -> Precedence
-- Pre: the operator has a binding in opTable
precedence op = let (pre, asso) = fromMaybe (0,N) (lookup op opTable) in pre--have that pre-condition so could use fromMaybe
--precedence op = head [pre | (chr,(pre,asso)) <- opTable, chr == op ]--alt. method:use listcomprehension to search through opTable and return the first element(result) in the list

associativity :: Operator -> Associativity
-- Pre: the operator has a binding in opTable
associativity op = let (pre, asso) = fromMaybe (0,N) (lookup op opTable) in asso
--associativity op = head [asso | (chr,(pre,asso)) <- opTable, chr == op ]

supersedes :: Operator -> Operator -> Bool
supersedes a b = (precedence a > precedence b) || ((precedence a == precedence b) && (associativity a == R))

tokenise :: String -> [Token]
-- Pre: The input string is a well-formed expression
tokenise [] = [] --base case
tokenise str@(c:cs)
  | isSpace c = tokenise cs --ignore space \n \t etc.
  | c `elem` ops = (TOp c) : (tokenise cs) --if c is an operator add TOp c to result
  | isDigit c = let (s1, s2) = break (not . isDigit) str in (TNum (stringToInt s1)) : (tokenise s2) --if c is digit,break the input to get the full digit in string form, covert it back to Int and deal with the rest
  | otherwise = let (s1, s2) = break notAlphaNumeric str in (TVar s1) : (tokenise s2) --if c is (part of) a name,break the input to get the full name and deal with the rest
  where notAlphaNumeric c = not (isDigit c || isAlpha c)

allVars :: String -> [String]
allVars str = nub [var | (TVar var) <- tokenise str] --use list comprehension to generate a list of all var names by pattern matching TVar and use nub to get rid of duplicates

--
-- This function is given
--
parseExpr :: String -> Expr
-- Pre: The input string is a well-formed expression
parseExpr s = parse (tokenise s) [] ['$']

-- Reminder: xxStack is actually list of xx
parse :: [Token] -> ExprStack -> OpStack -> Expr
-- Pre: The tokens and stacks collectively represent a
--      valid parser state.
parse [] [exp] _ = exp
parse [] (exp:exprs) (currentop:opstk) = parse [] newExpstk opstk
  where
  sndExpr = myhead exprs
  newExpr = EApp currentop exp sndExpr
  newExpstk = newExpr : mytail exprs

parse token@(TOp currentop:ts) expstk@(exp:exprs) opstk@(op:ops) = 
                                                 if supersedes currentop op then
                                                 parse ts exprs (currentop:opstk)
                                                 else
                                                 parse token newExpstk ops
                                                   where
                                                     sndExpr = myhead exprs
                                                     newExpr = EApp currentop exp sndExpr
                                                     newExpstk = newExpr : mytail exprs
parse token@(TVar varname:ts) expstk opstk = parse ts ((EVar varname):expstk) opstk
parse token@(TNum num:ts) expstk opstk = parse ts ((ENum num):expstk) opstk

myhead :: [a] -> a
--myhead [] = Nothing
myhead lst = head lst

mytail :: [a] -> [a]
mytail [] = []
mytail lst = tail lst
--[TNum 4,TOp '+',TVar "x",TOp '^',TNum 2,TOp '-',TNum 8,TOp '*',TVar "y"] [] ['$']