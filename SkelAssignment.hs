module SkelAssignment where

-- Haskell module generated by the BNF converter

import AbsAssignment
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  PBlocks blocks  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  BStm1 stm  -> failure x
  BStms stms  -> failure x


transPar :: Par -> Result
transPar x = case x of
  Para type' id  -> failure x


transFun :: Fun -> Result
transFun x = case x of
  Func type' id pars block  -> failure x


transStm :: Stm -> Result
transStm x = case x of
  SFunc fun  -> failure x
  SExp exp  -> failure x
  SValDec type' ids  -> failure x
  SValInit type' exps  -> failure x
  SReturn exp  -> failure x
  SReturnE  -> failure x
  SWhile exp block  -> failure x
  SIfElse exp block1 block2  -> failure x
  SPrint exp  -> failure x


transType :: Type -> Result
transType x = case x of
  TypeBool  -> failure x
  TypeInt  -> failure x
  TypeDouble  -> failure x
  TypeString  -> failure x
  TypeVoid  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EAssign id exp  -> failure x
  EDisjunction exp1 exp2  -> failure x
  EConjunction exp1 exp2  -> failure x
  EEq exp1 exp2  -> failure x
  ENeq exp1 exp2  -> failure x
  EGt exp1 exp2  -> failure x
  ELt exp1 exp2  -> failure x
  EGte exp1 exp2  -> failure x
  ELte exp1 exp2  -> failure x
  EAdd exp1 exp2  -> failure x
  ESub exp1 exp2  -> failure x
  EMul exp1 exp2  -> failure x
  EDiv exp1 exp2  -> failure x
  ENeg exp  -> failure x
  EPreInc exp  -> failure x
  EPreDec exp  -> failure x
  EPostInc exp  -> failure x
  EPostDec exp  -> failure x
  EId id  -> failure x
  EInt n  -> failure x
  ETrue  -> failure x
  EFalse  -> failure x
  EDouble d  -> failure x
  EString str  -> failure x
  EFunc id exps  -> failure x


