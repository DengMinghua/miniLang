

module AbsAssignment where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
data Program =
   PBlocks [Block]
  deriving (Eq,Ord,Show,Read)

data Block =
   BStm1 Stm
 | BStms [Stm]
  deriving (Eq,Ord,Show,Read)

data Par =
   Para Type Ident
  deriving (Eq,Ord,Show,Read)

data Fun =
   Func Type Ident [Par] Block
  deriving (Eq,Ord,Show,Read)

data Stm =
   SFunc Fun
 | SExp Exp
 | SValDec Type [Ident]
 | SValInit Type [Exp]
 | SReturn Exp
 | SReturnE
 | SWhile Exp Block
 | SIfElse Exp Block Block
 | SPrint Exp
  deriving (Eq,Ord,Show,Read)

data Type =
   TypeBool
 | TypeInt
 | TypeDouble
 | TypeString
 | TypeVoid
  deriving (Eq,Ord,Show,Read)

data Exp =
   EAssign Ident Exp
 | EDisjunction Exp Exp
 | EConjunction Exp Exp
 | EEq Exp Exp
 | ENeq Exp Exp
 | EGt Exp Exp
 | ELt Exp Exp
 | EGte Exp Exp
 | ELte Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | ENeg Exp
 | EPreInc Exp
 | EPreDec Exp
 | EPostInc Exp
 | EPostDec Exp
 | EId Ident
 | EInt Integer
 | ETrue
 | EFalse
 | EDouble Double
 | EString String
 | EFunc Ident [Exp]
  deriving (Eq,Ord,Show,Read)

