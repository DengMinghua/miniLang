{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintAssignment where

-- pretty-printer generated by the BNF converter

import AbsAssignment
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   PBlocks blocks -> prPrec i 0 (concatD [prt 0 blocks])


instance Print Block where
  prt i e = case e of
   BStm1 stm -> prPrec i 0 (concatD [prt 0 stm])
   BStms stms -> prPrec i 0 (concatD [doc (showString "{") , prt 0 stms , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Par where
  prt i e = case e of
   Para type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Fun where
  prt i e = case e of
   Func type' id pars block -> prPrec i 0 (concatD [prt 0 type' , prt 0 id , doc (showString "(") , prt 0 pars , doc (showString ")") , prt 0 block])


instance Print Stm where
  prt i e = case e of
   SFunc fun -> prPrec i 0 (concatD [prt 0 fun])
   SExp exp -> prPrec i 0 (concatD [prt 0 exp , doc (showString ";")])
   SValDec type' ids -> prPrec i 0 (concatD [prt 0 type' , prt 0 ids , doc (showString ";")])
   SValInit type' exps -> prPrec i 0 (concatD [prt 0 type' , prt 0 exps , doc (showString ";")])
   SReturn exp -> prPrec i 0 (concatD [doc (showString "return") , prt 0 exp , doc (showString ";")])
   SReturnE  -> prPrec i 0 (concatD [doc (showString "return") , doc (showString ";")])
   SWhile exp block -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 block])
   SIfElse exp block0 block -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 block0 , doc (showString "else") , prt 0 block])
   SPrint exp -> prPrec i 0 (concatD [doc (showString "print") , prt 0 exp , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Type where
  prt i e = case e of
   TypeBool  -> prPrec i 0 (concatD [doc (showString "bool")])
   TypeInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TypeDouble  -> prPrec i 0 (concatD [doc (showString "double")])
   TypeString  -> prPrec i 0 (concatD [doc (showString "string")])
   TypeVoid  -> prPrec i 0 (concatD [doc (showString "void")])


instance Print Exp where
  prt i e = case e of
   EAssign id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 1 exp])
   EDisjunction exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "||") , prt 2 exp])
   EConjunction exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "&&") , prt 3 exp])
   EEq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "==") , prt 4 exp])
   ENeq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "!=") , prt 4 exp])
   EGt exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">") , prt 5 exp])
   ELt exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<") , prt 5 exp])
   EGte exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">=") , prt 5 exp])
   ELte exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<=") , prt 5 exp])
   EAdd exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "+") , prt 6 exp])
   ESub exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "-") , prt 6 exp])
   EMul exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "*") , prt 7 exp])
   EDiv exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "/") , prt 7 exp])
   ENeg exp -> prPrec i 7 (concatD [doc (showString "-") , prt 8 exp])
   EPreInc exp -> prPrec i 8 (concatD [doc (showString "++") , prt 9 exp])
   EPreDec exp -> prPrec i 8 (concatD [doc (showString "--") , prt 9 exp])
   EPostInc exp -> prPrec i 9 (concatD [prt 10 exp , doc (showString "++")])
   EPostDec exp -> prPrec i 9 (concatD [prt 10 exp , doc (showString "--")])
   EId id -> prPrec i 11 (concatD [prt 0 id])
   EInt n -> prPrec i 11 (concatD [prt 0 n])
   ETrue  -> prPrec i 11 (concatD [doc (showString "true")])
   EFalse  -> prPrec i 11 (concatD [doc (showString "false")])
   EDouble d -> prPrec i 11 (concatD [prt 0 d])
   EString str -> prPrec i 11 (concatD [prt 0 str])
   EFunc id exps -> prPrec i 11 (concatD [prt 0 id , doc (showString "(") , prt 0 exps , doc (showString ")")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


