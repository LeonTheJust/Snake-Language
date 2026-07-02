{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- HLINT ignore "Use newtype instead of data" -}
import GHC.Base (VecElem(Int16ElemRep), bindIO)
import Data.Typeable
import System.IO
import Text.Read (readMaybe)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Data.Map (Map)
import Debug.Trace (trace)
import Data.Typeable
import Distribution.Compat.Lens (_1)
import qualified Data.Map as Map
import Distribution.PackageDescription (BuildInfo(extraLibFlavours))
import Distribution.Simple.Utils (xargs)
import Data.Time
import qualified Distribution.Compat.Prelude as LocalTime
import Control.Monad.RWS (evalRWS)
import Data.Time.Clock.POSIX

type NonTerminal = String
type Terminal = String

data Module =
    Module String (Map.Map String Value)

data Value
  = Int Int
  | String String
  | Functs AST AST
  | Arrays AST
  | Bool Bool
  | Arr [Value]
  deriving (Show , Eq)

data Token
  = TVar String
  | TNum Int
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TAssign
  | TRead
  | TPrint
  | TDsign
  | TEpsilon
  | TQuo
  | TStr String
  | TEq
  | TFdef String
  | TLPar
  | TRPar
  | IF
  | THEN
  | ELSE
  | ENDIF
  | DO
  | TEq'
  | Lt
  | Gt
  | TReturn
  | TLen
  | TImport
  | TRand
  deriving (Show, Eq)

--data Expr
-- = IntLit Int 
  -- | Var String 
 -- | Add Expr Expr
  -- | Mul Expr Expr
  -- Dix Expr Expr
--  | Sub Expr Expr
 -- deriving (Show, Eq)

--data Stmt
  -- = Assign String Expr
  -- Seq [Stmt]
  -- | Print Expr
  -- | Read Expr
  --deriving (Show, Eq)

--type Program = [Stmt]

data ParseError = ParseError String
  deriving (Show, Eq) -- gotta use this at some point :D

data AST
    = Assign AST AST
    | Read AST
    | Print AST
    | Add AST AST
    | Sub AST AST
    | Mul AST AST
    | Div AST AST
    | Var String
    | Num Int
    | Str String
    | F String
    | FunctDef AST AST AST
    | Funct AST
    | Array [AST]
    | If AST AST AST
    | Then AST
    | Else AST
    | Do AST AST
    | Block [AST]
    | Less AST AST
    | Greater AST AST
    | Equal AST AST
    | Free AST
    | Boolean Bool
    | Param [AST]
    | Return AST
    | Length AST
    | Import AST
    | Random AST
    deriving (Show, Eq)

data Stack a = Stack [a]
    deriving (Show,Eq)

print_top :: Stack AST -> IO()
print_top ast =
    print (top ast)

print_AST :: AST -> IO()
print_AST ast =
    print (ast)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)
pop (Stack []) = error "empty stack *pop*"

pop2 :: Stack a -> Stack a
pop2 (Stack (x:xs)) = Stack xs
pop2 (Stack []) = Stack []

empty :: Stack a -> Bool
empty (Stack xs) = null xs

top :: Stack a -> a
top (Stack (x:_)) = x
top (Stack [])   = error ("empty stack *top*")

getModuleName :: Module -> String
getModuleName (Module s map) = s

toSymbolAll :: [Token] -> Int ->[Symbol]
toSymbolAll tokens x =
    if x >= length tokens then
        []
    else
        ([toSymbol (tokens!!x)] ++ toSymbolAll tokens (x + 1))

toSymbol :: Token -> Symbol
toSymbol (TVar _)   = T "var"
toSymbol (TNum _)   = T "num"
toSymbol TPlus      = T "+"
toSymbol TMinus     = T "-"
toSymbol TMul       = T "*"
toSymbol TDiv       = T "/"
toSymbol TAssign    = T "<-"
toSymbol TRead      = T "read"
toSymbol TPrint     = T "print"
toSymbol TDsign     = T "$"
toSymbol TQuo       = T "'"
toSymbol (TStr s)   = T "str"
toSymbol TEq        = T "="
toSymbol (TFdef _)  = T "fdef"
toSymbol TLPar      = T "["
toSymbol TRPar      = T "]"
toSymbol IF         = T "if"
toSymbol THEN       = T "then"
toSymbol ELSE       = T "else"
toSymbol ENDIF      = T "endif"
toSymbol DO         = T "do"
toSymbol TEq'       = T "->"
toSymbol Gt         = T ">"
toSymbol Lt         = T "<"
toSymbol TReturn    = T "return"
toSymbol TLen       = T "length"
toSymbol TImport    = T "import"
toSymbol TRand      = T "random"

toSymbol2 :: String -> Symbol
toSymbol2 "var"  = T "var"
toSymbol2 "num"   = T "num"
toSymbol2 "+"      = T "+"
toSymbol2 "-"     = T "-"
toSymbol2 "*"      = T "*"
toSymbol2 "/"      = T "/"
toSymbol2 "<-"    = T "<-"
toSymbol2 "read" = T "read"
toSymbol2 "print"= T "print"
toSymbol2 "$"    = T "$"
toSymbol2 "'"    = T "'"
toSymbol2 "="    = T "="
toSymbol2 "]"    = T "]"
toSymbol2 "["    = T "["
toSymbol2 "if"   = T "if"
toSymbol2 "then" = T "then"
toSymbol2 "else" = T "else"
toSymbol2 "endif"= T "endif"
toSymbol2 "do"   = T "do"
toSymbol2 "->"   = T "->"
toSymbol2 ">"    = T ">"
toSymbol2 "<"    = T "<"
toSymbol2 "return"= T "return"
toSymbol2 "length"= T "length"
toSymbol2 "import"= T "import"
toSymbol2 "random"= T "random"

getTerminals :: [Terminal]
getTerminals =
    ["var" , "num", "+", "-","*", "/", "<-", "read", "print"
    , "$" ,"", "'", "str", "=", "fdef", "[", "]", "if", "then",
    "else", "endif", "do", "->", ">", "<", "return", "length", "import", "random"]

frt :: Grammar -> String -> Int-> Int ->[Terminal]
frt g sym x y=
    --if x >= length 
    if sym `elem` getTerminals then
        [sym]
    else if x >= length (getRules g sym 0 []) then
        []
    else if y >= length ((toStringArray ((getRules g sym 0 [])!!x) 0 [])) then
        [""]
    else if not ("" `elem` frt g (((toStringArray ((getRules g sym 0 [])!!x) 0 [])) !!y) 0 0)  then
        (frt g sym (x + 1) y ++ frt g (((toStringArray ((getRules g sym 0 [])!!x) 0 [])) !!0) 0 0)
    else if ("" `elem` frt g (((toStringArray ((getRules g sym 0 [])!!x) 0 [])) !!0) 0 0) then
        frt g sym x (y + 1)
    else
        []

first :: Grammar -> NonTerminal -> [Terminal]
first g nont = frt g nont 0 0

--flow :: Grammar -> String -> Int -> Int -> [Terminal]
--flow g sym x y =
   -- if sym == "" then
   --     ["$"]
  --  else if sym `elem` getTerminals then 
   --     []
   -- else if 

follow :: Grammar -> NonTerminal -> [Terminal]
follow g nt =
    if nt == "S" then
        ["$"]
    else if nt == "expr" then
        ["'" , "$", "]"]
    else if nt == "expr'" then
        ["$", "]"]
    else if nt == "term" then
        ["+" , "-" , "]", "$"]
    else if nt == "term'" then
        ["+" , "-" , "]","$"]
    else if nt == "x" then
        ["*" , "/" , "+", "-" , "]","$"]
    else if nt == "funct" then
        ["var", "num" , "'", "[" , "fdef"]
    else if nt == "multop" || nt == "addop" then
        ["var", "num"]
    else if nt == "y" then
        ["'"]
    else if nt == "param'" then
        ["]", "var", "num", "$"]
    else
        ["$"]

isFdef:: String -> Bool
isFdef s
  | isBlank s = False
  | (head s)`elem` "QWERTYUIOPASDFGHJKLZXCVBNM" && length s == 1 = True
  | (head s) `elem` "QWERTYUIOPASDFGHJKLZXCVBNM" && isVar (tail s) = True
  | otherwise = False

toToken :: String -> Token
toToken "read"  = TRead
toToken "print" = TPrint
toToken "+"     = TPlus
toToken "-"     = TMinus
toToken "*"     = TMul
toToken "/"     = TDiv
toToken "<-"    = TAssign
toToken "'"     = TQuo
toToken "="     = TEq
toToken "["     = TLPar
toToken "]"     = TRPar
toToken "if"    = IF
toToken "else"  = ELSE
toToken "then"  = THEN
toToken "endif" = ENDIF
toToken "do"    = DO
toToken "->"    = TEq'
toToken "<"     = Lt
toToken ">"     = Gt
toToken "return"= TReturn
toToken "length"= TLen
toToken "import"= TImport
toToken "random"= TRand
toToken s
  | isFdef s = TFdef s
  | isNumber s = TNum (read s)
  | isVar s    = TVar s
  | otherwise  = TStr s

type ParsingTable = (NonTerminal , Terminal , [Symbol])

type PTable = [ParsingTable]

getProds :: Grammar -> [Production]
getProds = productions

firstInRule :: Grammar -> [String] -> [String] -> Int -> [String]
firstInRule g rule s x=
    if x >= length rule then
        []
    else
        (((first g (rule !! x)) ++ (firstInRule g rule s (x + 1))))

f :: [String] -> String
f s = s!!0

rest :: [String] -> Int -> [String]
rest s x =
    if x >= length s then
        []
    else
        ([s!!x] ++ rest s (x + 1))

followInArray :: Grammar -> [String] -> Int -> [String]
followInArray g s x =
    if x >= length s then
        []
    else (followInArray g s (x + 1) ++ follow g (s!!x))

toStrRule :: Rule -> [String]
toStrRule (curr:symbols) 
    | null (curr:symbols) = []
    | otherwise = [toStringFromSymbol curr] ++ toStrRule symbols
toStrRule a = []


createProdTableTuple :: Grammar -> Rule -> NonTerminal -> [Terminal] -> Map.Map (NonTerminal, Terminal) [String] -> Map.Map (NonTerminal, Terminal) [String] 
createProdTableTuple g rule nont (curr:terms) ptable 
    | null (curr:terms) = ptable 
    | curr /= "" && curr `elem` first g nont && ( curr `elem` first g (toStringFromSymbol (rule!!0))  ) =
        if Map.member (nont, curr) ptable then error(show(rule) ++ " " ++ show(curr) ++ " " ++ show (rest (toStringArray rule 0 []) 1) ++ " " ++ show (followInArray g (rest (toStringArray rule 0 []) 1) 0))
        else (createProdTableTuple g rule nont terms (Map.insert (nont, curr) (toStrRule rule) ptable)) 
    | curr `elem`  first g nont && ( curr `elem`  first g (toStringFromSymbol (rule!!0)) || curr `elem`  (followInArray g (rest (toStringArray rule 0 []) 1) 0 ) ) = (createProdTableTuple g rule nont terms (Map.insert (nont , "$") (toStrRule rule) ptable)) 
    | otherwise = createProdTableTuple g rule nont terms ptable 
createProdTableTuple g rule nont a ptable = ptable 
createProdTablePart :: Grammar -> [Rule] -> NonTerminal -> Map.Map (NonTerminal, Terminal) [String] -> Map.Map (NonTerminal, Terminal) [String] 
createProdTablePart g (rule:rules) nont ptable 
    | null (rule:rules) = ptable 
    | otherwise = createProdTablePart g rules nont (createProdTableTuple g rule nont (getTerminals) ptable) 
createProdTablePart g a nont ptable = ptable 
createProdTable :: Grammar -> [NonTerminal] -> Map.Map (NonTerminal, Terminal) [String] -> Map.Map (NonTerminal, Terminal) [String] 
createProdTable g (currentnon : nonterminals) ptable 
    | null (currentnon, nonterminals) = ptable 
    | otherwise = createProdTable g nonterminals (createProdTablePart g (getRules g currentnon 0 []) currentnon ptable) 
createProdTable g a ptable = ptable


type Rule = [Symbol]

getRules :: Grammar ->  NonTerminal -> Int -> [Rule] -> [Rule]
getRules g nont x rule
  | x >= length (getProds g) = rule
  | fst (getProds g !! x)  ==  nont  = getRules g nont (x + 1) (rule ++ [(snd (getProds g !! x))])
  | otherwise = getRules g nont (x + 1) rule

data Symbol
  = T Terminal
  | N NonTerminal
  deriving (Eq, Show)

type Production = (NonTerminal, [Symbol])


toStringFromSymbol:: Symbol -> String
toStringFromSymbol (T s) = s
toStringFromSymbol (N s) = s

toStringArray :: [Symbol] -> Int -> [String] -> [String]
toStringArray s x arr=
    if x >= length s then
        arr
    else
        ([toStringFromSymbol (s!!x)] ++ toStringArray s (x + 1) arr)

data Grammar = Grammar
  { startSymbol :: NonTerminal
  , productions :: [Production]
  }

grammar :: Grammar
grammar = Grammar "S"
    [("S" , [T "var" , T "<-" , N "expr"]),
    ("S" , [T "->" , T "var"]),
    --("S" , [T "return" , N "x"]),
    ("S" , [T "do" , T "fdef" , T "[", N "param'"]),
    ("S" , [T "if" , N "expr''" , N "then'"]),
    ("S", [T "import" , T "fdef"]),
    ("then'", [T "then", N "S", N "else'"]),
    ("else'", [T "else", N "S", N "endif'"]),
    ("else'" , [N "S" , T "else'"]),
    ("endif'" , [T "endif"]),
    ("endif'" , [N "S", T "endif'"]), -- prepei na mporw na grapsw panw apo mia seira stis if gotta think of that frfr
    ("S" , [T "fdef" , N "='"]), -- maybe endif' -> endif | S endif' kai else' -> else' S | S or smth idkkkk type shi
   -- ("='" , [ T "[", N "param'", T "]", T "=" , N "expr"]),
    ("='" , [ T "[" ,N "param'", T "->" , N "S"]), -- prepei na prosthesw k edw block - tha brw tropo 
    ("param'" , [ T "]"]),
    ("param'" , [T "x'", N "param'"]),
    ("S" , [N "funct" , N "expr"]),
    ("expr" , [N "term", N "expr'"]),
    ("expr" , [T "length" , N "x"]),
    ("expr'" , [N "addop" ,N "term" , N "expr'" ]),
    ("expr" , [T "'" ,  N "y" , T "'"]),
    ("expr" , [T "do", T "fdef", T "[", N "param'"]),
    ("expr", [T "random", T "expr"]),
    ("expr'" , [T ""]),
    ("expr" , [T "fdef"]),
    ("expr''" ,[N "expr", N "log"]),
    ("log" , [T ">" , T "expr"]),
    ("log" , [T "<" , T "expr"]),
    ("log" , [T "=" , T "expr"]),
    --("expr" , [T "[" , N "expr" ,T "]"]),
    ("term" , [N "x" , N "term'"]),
    ("term'" , [N "multop" , N "x" , N "term'"]),
    ("term'" , [T ""]),
    ("x" , [T "var"]),
    ("x"  , [T "num"]),
    ("x", [T "[" , N "x" ,T "]"]),
    ("x'" , [N "x"]),
    ("x'" , [N "fdef"]),
    ("y" , [T "str"]),
    ("multop" , [T "*"]),
    ("multop" , [ T "/"]),
    ("addop" , [T "-" ]),
    ("addop" , [T "+"]),
    ("funct" , [T "read"]) ,
    ("funct", [ T "print"]),
    ("funct", [T "return"])]

isMarker :: String -> Bool
isMarker s =  s `elem` ["Mass", "Mfunct", "Mexpr", "Mexpr'", "Mterm",
 "Mterm'", "Mfunctdef", "MArr", "MIf", "MThen",
 "MElse", "MDo", "MS" ,"MGt" , "MLt", "MFree", "MEq" , "MParam", "MReturn", "MLen", "MImp", "MRand"]

pushMarker :: Stack String -> [String] -> Stack String
pushMarker stack rule
  | rule == ["do", "fdef" , "[", "param'"] = push "MDo" stack
  | rule == ["funct" , "'" , "y", "'"] = push "Mfunct" stack
  | rule == [ "var" ,  "<-" ,  "expr"] = push "Mass" stack
  | rule == [ "funct" , "expr"] = push "Mfunct" stack
  | rule == ["->" , "var"] = push "MFree" stack
  | rule == ["import" , "fdef"] = push "MImp" stack
  | rule == ["random", "expr"] = push "MRand" stack
 -- | rule == ["return"] = push "MReturn" stack
--  | rule == [ "term",  "expr'"] = push "Mexpr" stack
  | rule == [ "addop" , "term" ,  "expr'" ] = push "Mexpr'" stack
--  | rule == [ "x" ,  "term'"] = push "Mterm" stack 
  | rule == ["S" , "endif'"] = push "MS" stack
  | rule == ["S", "else'"] = push "MS" stack
 -- | rule == ["then" , "S", "else'"] = push "MS" stack
  | rule == ["multop" ,  "x" ,  "term'"] = push "Mterm'" stack
  | rule == ["param'",  "=" , "expr"] || rule == [ "[", "param'",  "->" ,  "S"] = push "Mfunctdef" stack
  | rule == [ "[" ,  "x" ,  "]"] = push "MArr" stack
  | rule == [ "if" ,  "expr''" , "then'"] = push "MIf" stack
  | rule == [">" , "expr"] = push "MGt" stack
  | rule == ["<" , "expr"] = push "MLt" stack
  | rule == ["=" , "expr"] = push "MEq" stack
  | rule == ["x'", "param'"] = push "MParam" stack -- na skeftw pws na to kanw na douleyei me 0 param!!
--  | rule == [ "then",  "S",  "else'"] = push "MThen" stack
  | rule == [ "else",  "S",  "endif'"] = push "MElse" stack
  | rule == ["length" , "x"] = push "MLen" stack
  | otherwise = stack

getNonTerminals :: [NonTerminal]
getNonTerminals =
    ["S" , "expr" , "funct" , "multop" , "addop" , "x" , "expr'", "term", "term'", "y", "expr''", "else'", "then'", "='", "endif'", "log", "param'", "x'"]

addRule :: Stack String -> [String] -> Int ->Stack String
addRule stack rule x
  | x == length rule = addRule (pushMarker stack rule) rule (x - 1)
  | x < 0 = stack
  | otherwise = addRule (push (rule!!x) stack) rule (x - 1)

slength :: Stack a -> Int
slength (Stack xs) = length xs

combine :: Stack AST ->String -> String->  Stack AST
combine ast marker inpt =
    let right = fst (pop ast) in
  if marker == "MS" then
    case top ast of
        (Block a) ->push (Block ( [(fst (pop (pop2 ast)))] ++ a)) (pop2 (pop2 ast))
        (Else a) ->
            let x = fst (pop ast) in
                push x ( push (Block ([fst (pop (pop2 (pop2 ast)))] ++ [fst (pop (pop2 ast))])) (pop2 (pop2  (pop2 ast))))
        a -> case fst (pop (pop2 ast)) of
            (Block b) -> push (Block ( b ++ [a])) (pop2 (pop2 ast))
            b -> push (Block ([b] ++ [a])) (pop2 (pop2 ast))
  else if marker == "MParam" then case top ast of
    (Param a) -> push (Param ( [(fst (pop (pop2 ast)))] ++ a)) (pop2 (pop2 ast))
    (a) -> push (Param [a]) ((pop2  ast))
  else if marker == "MRand" then push (Random (fst (pop ast))) (pop2 ast)
  else if marker == "MImp" then push (Import (top ast)) (pop2 ast)
  else if marker == "MFree" then push (Free (top ast)) (pop2 ast)
  else if marker == "MLen" then push (Length (top ast)) (pop2 ast)
  else if marker == "MElse" then push (Else (fst (pop ast))) (pop2 (pop2 ast))
  else if marker == "Mass" then push (Assign  (fst (pop (pop2 ast))) right) (pop2 (pop2 ast))
  else if marker == "Mfunct" && inpt == "print" then  push (Print right) (pop2 ast) -- gotta fixxx thattt
  else if marker == "Mfunct" && inpt == "read" then push (Read (fst (pop ast))) (pop2 ast)
  else if marker == "Mfunct" && inpt == "return" then push (Return (fst (pop ast))) (pop2 ast)
  else if marker == "Mexpr" && slength ast >= 2 && inpt == "+" then push (Add  (fst (pop (pop2 ast))) right) (pop2 (pop2 ast))
  else if marker == "Mexpr" && slength ast >= 2 && inpt == "-" then push (Sub (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "Mexpr"  then ast
  else if marker == "Mexpr'" && inpt == "+" then push (Add  (fst (pop (pop2 ast))) right) (pop2 (pop2 ast))
  else if marker == "Mexpr'"&& inpt == "-" then push (Sub (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "MGt" then push (Greater (fst (pop (pop2 ast))) (fst (pop ast))) (pop2 (pop2 ast))
  else if marker == "MLt" then push (Less (fst (pop (pop2 ast))) (fst (pop ast))) (pop2 (pop2 ast))
  else if marker == "MEq" then push (Equal (fst (pop (pop2 ast))) (fst (pop ast))) (pop2 (pop2 ast))
  else if marker == "Mexpr'"  then ast
  else if marker == "Mterm" && slength ast >= 2 && inpt == "*" then push (Mul (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "Mterm" && slength ast >= 2 && inpt == "/" then push (Div  (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "Mterm"  then ast
  else if marker == "Mterm'" && inpt == "*" then push (Mul  (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "Mterm'" && inpt == "/" then push (Div  (fst (pop (pop2 ast))) right ) (pop2 (pop2 ast))
  else if marker == "Mterm'"  then ast
  else if marker == "MIf" then push (If (fst (pop (pop2 (pop2 ast)))) (fst (pop (pop2 ast))) (fst (pop ast))) (pop2 (pop2 (pop2 ast)))
  else if marker == "MDo" then case (sizeofstack ast) of
    (1) ->push (Do right (Param [])) (pop2 ast)
    (_) ->push (Do (fst (pop (pop2 ast))) (fst (pop ast))) (pop2 (pop2 ast))
  --else if marker == "MThen"
  --else if marker == "MIf" then ast
  else if marker == "MArr" && canbeInArray (top ast) then push (Array [top ast]) (pop2 ast)
  else if marker == "MArr" then
    case ((fst (pop (pop2 ast))), (isNumber2 (fst (pop ast)))) of
        (Array x, True) -> push (Array (x ++ [(fst (pop ast))])) (pop2 (pop2 ast) )
        (Array x, False) -> push (Array (x ++ [( (fst (pop ast)))])) (pop2 (pop2 ast) )
        otherwise -> error ("idk" ++ inpt ++ show (ast)) -- change that idk what id do yet here
  else if marker == "MArr" && inpt == "]" then ast
  else if marker == "Mfunctdef" && (isParam (fst (pop (pop2 ast)))) then push (FunctDef (fst (pop  (pop2 (pop2 ast)))) (fst (pop (pop2 ast)))  right ) (pop2 (pop2 (pop2 ast)))
  else if marker == "Mfunctdef" then push (FunctDef ( (fst (pop (pop2 ast)))) (Param []) right ) (pop2  (pop2 ast))
  else error ("Wrong marker " ++ marker ++ " " ++ inpt ++ " " ++ show (slength ast))

sizeofstack :: Stack a -> Int
sizeofstack (Stack b) = length b

isParam :: AST -> Bool
isParam (Param a) = True
isParam ast = False

canbeInArray :: AST -> Bool
canbeInArray (Num a) = True
canbeInArray (Var a) = True
canbeInArray (Str a) = True
canbeInArray (_) = False

parser0::Grammar ->[String] -> Int ->Stack String -> Stack AST -> Stack String -> [String] -> Map.Map (NonTerminal, Terminal) [String] -> AST
parser0 g s x stack ast inpt s2 p
  | top stack == "" = parser0 g s x (pop2 stack) ast inpt s2 p
  | top stack == "$" && x >=length s = top ast
  | top stack == "$" && s!!x == "$" = top ast
  | top stack == "MArr" = parser0 g s x (pop2 stack) (combine ast (fst (pop stack))  (top inpt)) ( inpt) s2 p
  | isMarker (top stack) = parser0 g s x (pop2 stack) (combine ast (fst (pop stack))  (top inpt)) (pop2 inpt) s2 p
  | s!!x == "" = parser0 g s (x + 1) stack ast inpt s2 p
  | top stack == "$" && s!!x == "" = top ast
  | top stack `elem` getNonTerminals && s!!x == "$" && (Map.lookup  (top stack, "$" ) p)/= Nothing = parser0 g s x (pop2 stack) ast inpt s2 p --end of input - epsilon 
  | top stack `elem` getNonTerminals && (Map.lookup  (top stack , s!!x) p ) /= Nothing = 
    let (Just y) = Map.lookup  (top stack , s!!x) p in parser0 g s x (addRule (pop2 stack) y ((length y) ) ) ast inpt s2 p--reduce rule 
  | top stack `elem` getNonTerminals && (Map.lookup (top stack, "$") p) /= Nothing = parser0 g s x (pop2 stack) ast inpt s2 p --epsilon 
  | x>= length s = top ast
-- | top stack `elem` getTerminals && s!!x == top stack && (top stack == "[") = parser0 g s (x + 1) (pop2 stack) (push (Array []) ast) (push (s2!!x) inpt) s2 p
  | top stack `elem` getTerminals && s!!x == top stack && (top stack == "fdef") = parser0 g s (x + 1) (pop2 stack) (push (Var ((s2!!x))) ast) inpt s2 p -- match function definition
  | top stack `elem` getTerminals && s!!x == top stack && (top stack == "str") = parser0 g s (x + 1) (pop2 stack) (push (Str ((s2!!x))) ast) inpt s2 p-- match str 
  | top stack `elem` getTerminals && s!!x == top stack && (top stack == "endif" || top stack == "if" || top stack == "then" || top stack == "do") = parser0 g s (x + 1) (pop2 stack) ast inpt s2 p
  | top stack `elem` getTerminals && s!!x == top stack && isNumber (s2!!x) = parser0 g s (x + 1) (pop2 stack) (push (Num (read (s2!!x))) ast) inpt s2 p-- match int 
  | top stack `elem` getTerminals && s!!x == top stack && isVar (s2!!x) && (s2!!x) /= "length" && (s2!!x /= "do") && (s2!!x /= "random")= parser0 g s (x + 1) (pop2 stack) (push (Var (s2!!x)) ast) inpt s2 p -- match var 
  | top stack `elem` getTerminals && s!!x == top stack && (s!!x) `elem` ["+", "-" , "*", "/", "print", "read", "=", "return", "length", "do", "import", "random"] = parser0 g s (x + 1) (pop2 stack) ast (push (s2!!x) inpt) s2 p
  | top stack `elem` getTerminals && s!!x == top stack = parser0 g s (x + 1) (pop2 stack) ast inpt s2 p-- match anything else 
  | otherwise = error ("Syntax error at input" ++ show (s) ++ " " ++ "at" ++ "`" ++ (s!!x) ++ "`")

forceShow :: Show a => a -> String
forceShow x = length (show x) `seq` show x

findOp :: [String] -> Int -> Int
findOp s x
  | x >= length s = -1
  | s!!x == "+" || s!!x == "-" || s!!x == "*" || s!!x == "/" = x
  | otherwise = findOp s (x + 1)

--parseExpr :: [String] -> Int ->  [Expr]
--parseExpr s x
 -- | x >= length s = []
 -- | isNumber (s!!x) && x + 1 < length s = 
   -- if s!!(x + 1) == "+" && (isNumber (s!!x)) then ([Add (IntLit (toNum (s!!x))) (s!!(x + 2))] ++  parseExpr s (x + 3))
   -- else if s!!(x + 1) == "-" then (Sub s!!x s!!(x + 2) ++  parseExpr s (x + 3))
   -- else if s!!(x + 1) == "*" then (Mul s!!x s!!(x + 2) ++  parseExpr s (x + 3))
   -- else (Div s!!x s!!(x + 2) ++  parseExpr s (x + 3))

--parsePrint :: [Expr] -> Maybe Stmt
--parsePrint expr
 -- | expr /= [] = nothing
 -- | otherwise = Print expr

isBlank :: String -> Bool
isBlank s = all isSpace s || null s || s == ""

oneIsTrue :: [a -> Bool] -> a -> Bool
oneIsTrue functions x = length (filter ($ x) functions) == 1

acceptedPrefix :: String -> Bool
functs = ["read","print", "return"]
acceptedPrefix s= isFunctPrefix s||isNumber s|| isVar s || isOp s || isQuo s || isFdef s

isFunctPrefix  :: String -> Bool
isFunctPrefix s = any (isPrefixOf s) functs

toNum :: String ->  Int
toNum = read

isNumber :: String -> Bool
isNumber s = case readMaybe s :: Maybe Int of
    Just _ -> True
    Nothing -> False

isNumber2 :: AST -> Bool
isNumber2 (Num x) = True
isNumber2 ast = False

isVar :: String -> Bool
isVar s = not (any ( `elem` "+-/*^%!@#^&*()=[],;:12>345<67890 '\"") s) && notElem s functs && s /= " " && s /= "" && not ((head s) `elem` "QWERTYUIOPASDFGHJKLZXCVBNM")

isOp :: String -> Bool
isOp s = s `elem`  ["<-" , "*" , "^" , "+", "-", "/" , "!" , "=", "->" , ">", "<"]

isFunct :: String -> Bool
isFunct s = s `elem` functs

isQuo :: String -> Bool
isQuo "'" = True
isQuo "[" = True
isQuo "]" = True
isQuo s = False

accepted :: String -> Bool
accepted = oneIsTrue [isNumber, isOp, isFunct, isVar ,isQuo, isFdef]

accept :: String -> IO()
accept s = do
    if isFunct s then do
        print ("Accepted Function " ++ s)
    else if isNumber s then do
        print ("Accepted Number " ++ s)
    else if isVar s then do
        print ("Accepted Variable " ++ s)
    else do
        print ("Accepted Operator " ++ s)

charToStr :: Char -> String
charToStr c = [c]

deny :: String -> IO()
deny s = do
    if s == "" then
        return ()
    else
        print ("Denied " ++ s)

lexx :: String -> String -> Bool -> [Token]
lexx (head:ta) current str
    | null (head:ta) = [toToken current]
    | head == ' ' && not str && current /= "" = toToken current : lexx ta "" str
    | head == ' ' && (current == "" || current == " ") = lexx ta "" str
    | head == '\'' && not str = TQuo : lexx ta "" True
    | head == '\'' = TStr current : TQuo : lexx ta "" False
    | str = lexx ta (current ++ [head]) str
    | acceptedPrefix ( current ++ [head]) = lexx ta ( current ++ [head]) str
    | accepted (current ++ [head]) = toToken (current ++ [head]) : lexx ta ""  str
    | accepted current = toToken current : lexx (head:ta) "" str
lexx t current str
    | null t = []
    | otherwise = error ("invalid token " ++ (show t) ++ " " ++ show current)

scd :: (NonTerminal , Terminal , Rule) -> Terminal
scd (nont , t , r) = t
scd ("" , "" , []) = ""
scd null = ""

frst :: (NonTerminal , Terminal , Rule) -> NonTerminal
frst (nont, t ,r) = nont
frst ("" , "" , []) = ""
frst null = ""

thrd :: (NonTerminal , Terminal , Rule) -> Rule
thrd (nont, t ,r) = r
thrd ("" , "" , []) = []
thrd null = []

findInParsinTable :: Grammar -> NonTerminal -> Terminal -> PTable -> Int ->[String]
findInParsinTable g nont term p x=
    if x >= length p then
        []
    else if frst (p!!x) == nont && scd (p!!x) == term then
        toStringArray (thrd (p!!x)) 0 []
    else
        findInParsinTable g nont term p (x+1)

toStringFromToken :: Token -> String
toStringFromToken token = case token of
    (TVar name) -> name
    (TNum num )-> (show (num))
    TPlus -> "+"
    TMinus -> "-"
    TMul -> "*"
    TDiv -> "/"
    TAssign -> "<-"
    TRead -> "read"
    TPrint -> "print"
    TDsign -> "$"
    TEpsilon -> ""
    TStr s -> s
    TQuo -> "'"
    TEq -> "="
    (TFdef f) -> f
    TRPar -> "]"
    TLPar -> "["
    IF -> "if"
    ELSE -> "else"
    THEN -> "then"
    ENDIF -> "endif"
    TEq' -> "->"
    Gt -> ">"
    Lt -> "<"
    TReturn -> "return"
    TLen -> "length"
    DO -> "do"
    TImport -> "import"
    TRand -> "random"

toStrArray :: [Token] -> Int -> [String]
toStrArray tokens x
  | x >= length tokens = []
  | otherwise = ([toStringFromToken (tokens!!x)] ++ toStrArray tokens (x + 1))

multStr :: String -> Int -> Int -> String
multStr s x y
  | x <= y = s
  | otherwise = (multStr s x (y + 1) ++ s)

pop3 :: [AST] -> Value
pop3 (x:xs) = Arrays (Array (xs))
pop3 [] = Int (-1)

toBoolean :: Maybe Value -> Value
toBoolean (Just (Bool a)) = Bool a
toBoolean v = error "not bool"

eval :: AST -> Map.Map String Value  -> Maybe Value
eval (Mul a b) map = case (eval a map, eval b map) of
    (Just (Int x), Just (Int y)) -> Just (Int (x * y))
    (Just (String a), Just (Int b)) -> Just (String (multStr a b 1))
    (Just (Arrays (Array a)) , Just (Int b)) -> (Just  (pop3 a))
    _ -> Nothing
eval (Div a b ) map = case  (eval a map , eval b map) of
    (Just (Int x), Just (Int y))  -> Just (Int (x `div` y))
    (Just (Arrays(Array a)) , Just (Int b) ) ->  (eval (a!!b) map)
    _ -> Nothing
eval (Add a b ) map = case (eval a map, eval b map) of
    (Just (Int x), Just (Int y)) -> Just (Int (x + y))
    (Just (String x), Just (String y)) -> Just (String ( (filter (`notElem` "'") ( x ++ y))))
    (Just (Arrays(Array a)) , Just (Arrays (Array b)) ) -> Just (Arrays (Array (a ++ b)))
    (Just (Arrays(Array a)) , Just (Int b) ) -> Just (Arrays (Array (a ++ [Num b])))
    _ -> Nothing
eval (Sub a b ) map = case (eval a map, eval b map) of
    (Just (Int x), Just (Int y)) -> Just (Int (x - y))
    _ -> Nothing
eval (Num a) map = Just (Int a)
eval (Boolean a) map = Just (Bool a)
eval (Var a) map = case  Map.lookup a map of
    Just (String a) -> Just (String (filter (`notElem` "'") a))
    Just (Int a) -> Just (Int a)
    Just (Functs a b) -> Just (Functs a b)
    Just (Arrays a) -> Just (Arrays a)
    Just (Bool a) -> Just (Bool a) 
    Nothing -> error ("Variable " ++ show(a) ++ " not defined or not in scope")
eval (Funct a) map = Just (Int 1)
eval (FunctDef a param b) map = Just (Functs a b)
eval (Str a) map = Just (String a)
eval (Array a) map = Just (Arrays (Array a))
eval (Greater (Num a) (Array b)) map = Just (Bool False)
eval (Greater a b) map = case ( ((eval a map), (eval b map))) of
    (Just (Int x),  Just (Int y)) -> Just (Bool (x > y))
    (Just (Int x) , _) -> Just (Bool False)
    _ -> error (show ((eval a map), (eval b map)))
eval (Less a b) map = case ( ((eval a map), (eval b map))) of
    (Just (Int x),  Just (Int y)) -> Just (Bool (x < y))
    (Just (Int x) , _) -> Just (Bool True)
    _ -> error (show ((eval a map), (eval b map)))
eval (Equal a b) map = case ( ((eval a map), (eval b map))) of
    (Just (Arrays x) , Just (Arrays y)) -> Just (Bool (x == y))
    (Just (Int x),  Just (Int y)) -> Just (Bool (x == y))
    (Just (Int x) , _) -> Just (Bool False)
    _ -> error (show ((eval a map), (eval b map)))
eval (Length a) map = case (eval a map) of
    (Just (Arrays (Array x))) -> Just (Int (length x))
    (Just (String x)) -> Just (Int (length x))
    _ -> error ("function 'length' only works for type Array")
eval (Random ast) map = 
    case ((eval ast map), (Map.lookup "seed" map))of
        ((Just (Int a)), Just(Int seed)) -> Just (Int (mod seed a))
        _ -> error ("Variable " ++ show(ast) ++ " not defined or out of scope")
--eval (Do (Var a) param) map =
  --  let newmap <- compile 
eval ast map = error (show (ast))
--eval (Do (FunctDef a b))

evalArr :: AST -> Map.Map String Value  -> [Maybe Value]
evalArr (Array (x:xs)) map =
    ([eval x map] ++ evalArr (Array xs) map)
evalArr (Array []) map =
    []
evalArr (Array (x:_)) map =
    [eval x map]
evalArr (Var a) map = [eval (Var a) map]

evalStr :: AST -> Map.Map String Value -> Maybe Value
evalStr (Var a) map = case Map.lookup a map of
    Just (String a) -> Just (String a)
    Just (Functs a b)  -> (eval a map) -- check this too
    Nothing -> Nothing
evalStr (Str a) map = Just (String a)
evalStr (Var a) map = error ("Variable " ++ a ++ " not defined")
evalStr (Add a b) map = case (evalStr a map, evalStr b map) of
    (Just (String x), Just (String y)) -> Just (String ( (filter (`notElem` "'") ( x ++ y))))
    _ -> Nothing

getVar :: AST -> String
getVar (Var a) = a
getVar (FunctDef (Var a) param b) = a
--getVar (Array a) = 
getVar ast = error ("Var " ++ show (ast) ++ " Not found")

toNumber :: Maybe Value -> Int
toNumber x = case x of
    Just (Int x) -> x
    _ -> error ("invalid input" ++ show x)


isNumM :: Maybe Value -> Bool
isNumM x = case x of
    Just (Int x) -> True
    otherwise -> False

isFunctT :: Maybe Value -> Bool
isFunctT f = case f of
    Just (Functs a b) -> True
    otherwise -> False

isArray :: Maybe Value -> Bool
isArray a = case a of
    Just (Arrays a) -> True
    otherwise -> False

toStr :: Maybe Value -> String
toStr s = case s of
    Just (String s) -> s
    Just (Int a) -> show a
    _ -> error "invalid input str"

toStrAst :: AST -> String
toStrAst (Str s) = s
toStrAst ast = error "not a string"

isStr :: String -> Bool
isStr s
    | (s!!0) == '\'' && (s!!(length s - 1)) == '\'' = True
    | otherwise = False

foundStr :: String -> Map.Map String String -> Bool
foundStr s map = case Map.lookup s map of
    Just s -> True
    Nothing -> False

isStr2 :: Maybe Value -> Bool
isStr2 (Just (String s)) = True
isStr2 v = False

hasIf :: Maybe Value -> Bool
hasIf (Just (Functs (If a b c) param)) = True
hasIf v = False

getString :: AST -> String
getString (Str s) = s
getString ast = error "not string?"

toArr :: [Maybe Value] -> [Value]
toArr [Just a] = [a]
toArr [ast] = [error "A"]

match :: AST -> AST -> Map.Map String Value -> Map.Map String Value -> Map.Map String Value
match (Param ((Var x):xs)) (Param ((Var y):ys)) map local =
    case (eval (Var x) map) of
        (Just a) -> Map.insert y a (match (Param (xs)) (Param (ys)) map local)
        (Nothing ) -> error ("Variable " ++ show ((Param ((Var x):xs))) ++ " not defined")
match (Param ((Var x):_)) (Param ((Var y):_)) map local=
    case (eval (Var x) map) of
        (Just a) -> Map.insert y a local
        (Nothing ) -> error ("Variable " ++ x ++ " not defined")
match (Param []) (Param []) map local = 
    let Just s = (Map.lookup "seed" map) in
         Map.insert "seed" s local
match (Param ((Funct x):xs)) (Param ((Var y):ys)) map local =
    case (eval (Funct x) map) of
        (Just a) -> Map.insert y a (match (Param (xs)) (Param (ys)) map local)
        (Nothing ) -> error ("Variable " ++ show ((Param ((Funct x):xs))) ++ " not defined")
match (Param ((Funct x):_)) (Param ((Var y):_)) map local=
    case (eval (Funct x) map) of
        (Just a) -> Map.insert y a local
        (Nothing ) -> error ("Variable " ++ show x ++ " not defined")
match (Param ((Num x) : xs)) (Param ((Var y) :ys)) map local =
    Map.insert y (Int x) (match (Param (xs)) (Param (ys)) map local)
match (Param ((Num x) : _)) (Param ((Var y) :_)) map local =
    Map.insert y (Int x) local
match (Param ((Str x) : xs)) (Param ((Var y) :ys)) map local =
    Map.insert y (String x) (match (Param (xs)) (Param (ys)) map local)
match (Param ((Str x) : _)) (Param ((Var y) :_)) map local =
    Map.insert y (String x) local
match (Param ((Array x) : xs)) (Param ((Var y) :ys)) map local =
    Map.insert y (Arrays (Array x)) (match (Param (xs)) (Param (ys)) map local)
match (Param ((Array x) : _)) (Param ((Var y) :_)) map local =
    Map.insert y (Arrays (Array x)) local
match ast1 ast2 map1 map2 = error ((show ast1) ++ (show ast2))

thrd2 :: (a,b,c) -> c
thrd2 (a,b,c) = c

compile :: AST -> Map.Map String Value -> Map.Map String Value-> Map.Map String Module->IO (Map.Map String Value , Map.Map String Value,Map.Map String Module)
compile (Print a) map ftable modules= do
    --print ( map)
    if(isNumM (eval a map)) then do -- check again mporei na exw lathoss
        print (toNumber (eval a ( map))) -- lowk basically kanw box k unbox me ton kataskebasth Value
        return (map,ftable,modules)
    else if (isFunctT (eval a map)) then do
        print ( show (eval a (map)) ++ "a")
        return (map, ftable,modules)
    else if (isStr2 (eval a map)) then do
        print (toStr (eval a map))
        return (map, ftable,modules)
    else if (isArray (eval a map)) then do
        let [(Just (Arrays (Array y)))] = (evalArr a map)
        print y
        return (map, ftable,modules)
    else do
        if (eval a map) /= Nothing then
            print (eval a map)
        else
            error ("Variable " ++ show a ++ " not defined or is not in scope")
        return (map, ftable,modules)
compile (Assign a (Do functdef param)) map ftable modules= case ( eval (Do functdef param) ( map)) of
    (Just y) -> return ((Map.insert (getVar a) y ( map)), ftable,modules)
    _ -> case (eval (Do functdef param) ( map)) of
        (Just x) -> return ((Map.delete (getVar a) ( map)),ftable,modules)
        _ -> error (show (eval (Do functdef param) ( map)))
compile (Assign a b) map ftable modules= case ( eval b ( map)) of
    (Just y) -> return ((Map.insert (getVar a) y ( map)) ,ftable,modules)
    _ -> case (eval b ( map)) of
        (Just x) -> return (Map.delete (getVar a) ( map) , ftable,modules)
        _ -> error (show (eval b ( map)))
compile (Read a) map ftable modules= do
    inpt <- getLine
    if (isNumber inpt) then do
        --let newast = Assign a (Num (read(inpt))) i
        let newmap = (Map.insert (getVar a) (Int (read (inpt))) ( map))
        return (newmap,ftable,modules)
    else if (isStr inpt) then do
        let newmap = (Map.delete (getVar a) ( map))
        return (newmap, ftable,modules)
    else do
        let newmap = (Map.insert (getVar a) (String (toStr (eval (Var inpt) ( map)))) ( map ))
        return (newmap, ftable,modules)
compile (FunctDef a param  b) map ftable modules = do
    let newmap = (Map.insert (getVar a) (Functs b param) ftable)
    --print newmap
    return (map,newmap,modules)
compile (If a (Block b) (Else (Block c))) map ftable modules=
    if ( (eval a map)) == (Just (Bool True))  then do
        newmap <- compileBlock b map ftable modules
        return (newmap)
    else do
        newmap <- compileBlock c map ftable modules
        return (newmap)
compile (If a (Block b) (Else  c)) map ftable modules=
    if ( (eval a map)) == (Just (Bool True)) then do
        newmap <- compileBlock b map ftable modules
        return newmap
    else do
        newmap <- compile c map ftable modules
        return (newmap )
compile (If a b (Else (Block c))) map ftable modules=
    if ( (eval a map)) == (Just (Bool True))  then do
        newmap <- compile b map ftable modules
        return newmap
    else do
        newmap <- compileBlock c map ftable modules
        return newmap
compile (If a b (Else c)) map ftable modules= do
    if ( (eval a map)) == (Just (Bool True)) then do
        newmap <- compile b map ftable modules
        return newmap
    else do
        newmap <- compile c map ftable modules
        return newmap
compile (Do (Var a) (Param param)) map ftable modules = do
    let k = Map.lookup a ftable
    if k /= Nothing then do
        let (Functs x param2 ) = evalFunct (Do (Var a) (Param param)) ftable
        let newmap = match (Param param) param2 map Map.empty
        newmap2 <- compile x newmap ftable modules
        --print newmap2
        if (Map.lookup "returned" (fst2 newmap2)) /= Nothing then do
            let Just z = Map.lookup "returned" (fst2 newmap2)
            let newmap3 = Map.insert a z map
            --print newmap3
            return (newmap3, snd2 newmap2, thrd2 newmap2)
        else
            return (map, ftable,modules)
        --print (Functs b)
        --return (removeLocal param2 newmap2)
    else do
        let s = getModule a
        let (Just (Module name mmap)) = Map.lookup s modules
        let (Functs x param2 ) = evalFunct (Do (Var a) (Param param)) mmap
        let newmap = match (Param param) param2 map Map.empty
        newmap2 <- compile x newmap ftable modules
        --print newmap2
        if (Map.lookup "returned" (fst2 newmap2)) /= Nothing then do
            let Just z = Map.lookup "returned" (fst2 newmap2)
            let newmap3 = Map.insert a z map
            --print newmap3
            return (newmap3, snd2 newmap2, thrd2 newmap2)
        else
            return (map, ftable,modules)
        --print (Functs b)
        --return (removeLocal param2 newmap2)
compile (Free (Var a)) map ftable modules= do
    let newmap = Map.delete a map
    return (newmap,ftable,modules)
compile (Block a ) map ftable modules =
    compileBlock a map ftable modules
compile (Return a) map ftable modules=
    return ((Map.insert "returned" (toValue (eval a map)) map), ftable,modules)
compile (Import (Var filename)) map ftable modules= do
    r <- compileFile ( filename++ ".txt")
    return (map, ftable, (Map.insert filename r modules))
compile ast map ftable modules = error ( "1" ++ show ast ++ "a")

getModule :: String -> String
getModule (x:xs) =
    if x == '.' then
        ""
    else
        ([x] ++ getModule xs)
getModule (_:_) = error "no module name where its supposed to be lowkey"

toValue :: Maybe Value -> Value
toValue (Just a) = a
toValue (Nothing) = error "value not defined"

removeLocal :: AST -> Map.Map String Value -> Map.Map String Value
removeLocal (Param ((Var x):xs)) map =
    Map.delete x (removeLocal (Param xs) map)
removeLocal (Param ((Var x):_)) map =
    Map.delete x map
removeLocal (Param ([])) map = map -- useless funct at this point xd

fst2 :: (a,b,c) -> a
fst2 (a,b,c) = a

snd2 :: (a,b,c) -> b
snd2 (a,b,c) = b

compileBlock :: [AST] -> Map.Map String Value -> Map.Map String Value -> Map.Map String Module-> IO( Map.Map String Value , Map.Map String Value, Map.Map String Module)
compileBlock (x:xs) map ftable modules= do
    newmap <- compile x map ftable modules
    compileBlock xs (fst2 newmap) (snd2 newmap) (thrd2 newmap)
compileBlock (x:_) map ftable modules = do
    newmap<- compile x map ftable modules
    return (newmap)
compileBlock (_:_) map ftable modules=
    return (map,ftable,modules)
compileBlock [] map ftable modules =
    return (map,ftable,modules)

evalFunct :: AST -> Map.Map String Value -> Value
evalFunct (Do (Var a) b) map =
    case (Map.lookup a map) of
    Just (ast) -> ast
    Nothing -> error ("No function by the name of" ++ a)

compileLinesFromImport :: Grammar ->  Map.Map (NonTerminal, Terminal) [String]-> Handle -> Map.Map String Value -> IO (Map.Map String Value)
compileLinesFromImport g p h map= do
    end <- hIsEOF h
    if not end then do
        s <- hGetLine h
        let tokens = lexx (Prelude.tail s ++ " ") [Prelude.head s] False
        let tokens2 = ((toStringArray (toSymbolAll tokens 0) 0 []))
        let stack = Stack ["S","$"]
        let stack2 = Stack []
        let stack3 = Stack []
        let (FunctDef a param b) = (parser0 g (tokens2 ++ ["$"]) 0 stack stack2 stack3 (toStrArray tokens 0) ) p
        let r = (Map.insert (getVar a) (Functs b param) map)
        k <- compileLinesFromImport g p h r
        return (k)
    else
        return map

compileFile :: String -> IO Module
compileFile filename = do
    let g = grammar
    let p = createProdTable g getNonTerminals (Map.empty)
    h <- openFile filename ReadMode
    r <- compileLinesFromImport g p h Map.empty
    return (Module filename r)

readMore :: IO ([Token] , [String])
readMore = do
    s <- getLine
    let tokens = lexx (Prelude.tail s ++ " ") [Prelude.head s] False
    let tokens2 = ((toStringArray (toSymbolAll tokens 0) 0 []) )
    if "endif" `elem` tokens2 then
        return (tokens, (tokens2 ++ ["$"]))
    else do
        x<- readMore
        return (tokens ++ (fst (x)), tokens2 ++ (snd (x)))

createRandom :: IO Int
createRandom = do
    localTime <- getZonedTime
    -- "%q" outputs exactly 12 digits (picoseconds). 
    -- Taking the first 3 digits gives you the milliseconds.
    let ms = take 3 (formatTime defaultTimeLocale "%q" localTime)
    let s = take 2 (formatTime defaultTimeLocale "%q" localTime)
    let ms2 = read (ms) :: Int
    let s2 = read (s) :: Int
    return (ms2*s2)

getCode :: (Map.Map String Value , Map.Map String Value, Map.Map String Module) -> Grammar -> Map.Map (NonTerminal, Terminal) [String] -> IO()
getCode mapp g p = do
    s <- getLine
    t <- getPOSIXTime
    let seed = floor (t * 1000000)
    let map = (Map.insert "seed" (Int seed) (fst2 mapp), snd2 mapp, thrd2 mapp)
    if s /= "end"  then do
        --print (charToStr (head s))
        let tokens = lexx (Prelude.tail s ++ " ") [Prelude.head s] False
        --print tokens
        let tokens2 = ((toStringArray (toSymbolAll tokens 0) 0 []))
        if "if" `elem` tokens2 && not ("endif" `elem` tokens2) then do
            x <- readMore
            let tokens3 = tokens ++ (fst x)
            let tokens4 = tokens2 ++ (snd x)
            --print ((show( tokens3)) ++ (show (tokens4)))
            let stack = Stack ["S","$"]
            let stack2 = Stack []
            let stack3 = Stack []
            let ing = (parser0 g tokens4 0 stack stack2 stack3 (toStrArray tokens3 0) ) p
            --print ing
            newmap <- compile ing (fst2 map) (snd2 map) (thrd2 map)
            getCode newmap g p
        else do
            --print (toSymbolAll tokens 0)
            --print ((toSymbolAll tokens 0))
            --print tokens2
            --print (p)
            --print (first g "='")
            --print (toStringArray (toSymbolAll tokens 0) 0 [] )
            --print (follow g "funct")
            --print (findInParsinTable g "='" "->" p 0)
            let stack = Stack ["S","$"]
            --print (top stack3)
            --print (findInParsinTable g "expr'" "$" p 0)
            --print tokens2
            let stack2 = Stack []
            let stack3 = Stack []
            --print ((toStrArray tokens 0))
            --let ing = (parser0_debug g tokens2 0 stack stack2 stack3 )
            let ing = (parser0 g (tokens2 ++ ["$"]) 0 stack stack2 stack3 (toStrArray tokens 0) ) p
            --print (ing)
            newmap <- compile ing (fst2 map) (snd2 map) (thrd2 map)
            --print (newmap)
            --print (newmap )
            --print "Do you want to continue? [Y/N]"
            getCode newmap g p
    else
        return ()

main :: IO()
main = do
    print "Snake version pre alpha 1.0"
    let g = grammar
    --print (toStringArray ((getRules g "expr" 0 [])!!0) 0 [])
    let p = createProdTable g getNonTerminals (Map.empty)
    --print p
    --let map = Map.insert "LenW" (Functs (If (Greater (Num 0) (Var "inpt")) (Assign (Var "length") (Var "length")) (Else (Block [Assign (Var "length") (Add (Var "length") (Num 1)),Assign (Var "inpt") (Mul (Var "inpt") (Num 1)),Do (Var "LenW")])))) Map.empty
    --let map0 = Map.insert "Length" (Functs (Block [Assign (Var "length") (Num (-1)), Do (Var "LenW"), Free (Var "inpt")])) map
    --let map1 = Map.insert "Min" (Functs (Block [Assign (Var "inpt1") (Var "inpt") ,Do (Var "Length") , Assign (Var "inpt") (Var "inpt1"), Free (Var "inpt1") ,If (Greater (Num 1) (Var "length")) (Assign (Var "min") (Var "min")) (Else (Block [Assign (Var "temp") (Div (Var "inpt") (Num 0)),If (Greater (Var "min") (Var "temp")) (Block [Assign (Var "min") (Div (Var "inpt") (Num 0)),Block [Assign (Var "inpt") (Mul (Var "inpt") (Num 1)),Do (Var "Min")]]) (Else (Block [Assign (Var "inpt") (Mul (Var "inpt") (Num 1)),Do (Var "Min")]))]))])) map0
    --let map2 = Map.insert "FindMin" (Functs (Block [Assign (Var "min") (Num 1000000000) , Do (Var "Min"), Free (Var "inpt")])) map1
    --let map3 = Map.insert "Max" (Functs (Block [Assign (Var "inpt1") (Var "inpt") ,Do (Var "Length") , Assign (Var "inpt") (Var "inpt1"), Free (Var "inpt1") ,If (Greater (Num 1) (Var "length")) (Assign (Var "max") (Var "max")) (Else (Block [Assign (Var "temp") (Div (Var "inpt") (Num 0)),If (Greater (Var "temp") (Var "max")) (Block [Assign (Var "max") (Div (Var "inpt") (Num 0)),Block [Assign (Var "inpt") (Mul (Var "inpt") (Num 1)),Do (Var "Max")]]) (Else (Block [Assign (Var "inpt") (Mul (Var "inpt") (Num 1)),Do (Var "Max")]))]))])) map2
    --let map4 = Map.insert "FindMax" (Functs (Block [Assign (Var "max") (Num (-1000000000)) , Do (Var "Max"), Free (Var "inpt")])) map3
   -- let map5 = Map.insert "Sum" (Functs (Block [Assign (Var "sum") (Num 0) , Do (Var "S/"), Free (Var "inpt")])) map4
   -- let map6 = Map.insert "S/" (Functs (Block [Assign (Var "inpt1") (Var "inpt"), Do (Var "Length") ,Assign (Var "inpt") (Var "inpt1"), If (Greater (Num 1) (Var "length")) (Assign (Var "sum") (Var "sum")) (Else (Block [Assign (Var "sum") (Add (Var "sum") (Div (Var "inpt") (Num 0))) , Assign (Var "inpt") (Mul (Var "inpt") (Num 1)), Do (Var "S/")]))]) ) map5
   --let map7 = Map.insert "S1" (Functs (If (Equal (Var "target") (Div (Var "inpt") (Num 0))) (Assign (Var "found") (Boolean True)) (Else (Block [Assign (Var "inpt") (Mul (Var "inpt") (Num 1) ) , Do (Var "S1")])))) map6
   --let map8 = Map.insert "Search" (Functs (Block [Assign (Var "found") (Boolean False), Do (Var "S1")])) map7 -- alagh for case not found
    getCode (Map.empty,Map.empty,Map.empty) g p
