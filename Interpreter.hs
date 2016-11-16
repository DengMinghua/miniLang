module Interpreter where

-- interpreter for a calculator with "memory" (variable assignments)

import AbsAssignment
import PrintAssignment

import Control.Monad.State
import qualified Data.Map.Strict as Map

-- the interpreter

data InternalType =
  IInteger Integer
  | IDouble Double
  | IString String
  | IVoid
  deriving (Eq, Ord, Read, Show)

execProgram :: Program -> Action Void
execProgram p = do
  case p of
    PBlocks bs -> do
      recExecBlock bs
  funcs <- gets functions
  case Map.lookup (Ident "main") funcs of
    Just (Func t i p b) -> do
        modify (\s -> s{variables = Map.empty : (variables s)})
        v <- execBlock b
        modify (\s -> s{variables = tail(variables s)})
        return ()
    Nothing -> return ()

recExecBlock :: [Block] -> Action Void
recExecBlock bs = do
  case bs of
    (b:lbs) -> do
      execBlock b
      recExecBlock lbs
      return ()
    [] -> do
      return ()

execBlock :: Block -> Action (Maybe InternalType)
execBlock b = case b of
  BStm1 s-> do
    case s of
      SReturn e -> do
        v <- eval e
        return (Just v)
      SReturnE -> do
        return (Just IVoid)
      _ -> do
        execStm s
        return Nothing
  BStms ss-> do
    recExecStms ss

recExecStms :: [Stm] -> Action (Maybe InternalType)
recExecStms ss  = do
  case ss of
    s: lss -> do
      case s of
        SReturn e -> do
          v <- eval e
          return (Just v)
        SReturnE -> do
          modify (\s -> s{variables = tail(variables s)})
          return (Just IVoid)
        _ -> do
          es <- execStm s
          case es of
            Nothing -> do
              v <- recExecStms lss
              return v
            (Just x) -> do
              return es
    [] -> do
      return Nothing

execStm :: Stm -> Action (Maybe InternalType)
execStm s = case s of
  SExp  e -> do
    eval e
    return Nothing
  SValDec t ids -> do
    forEach ids updateVarInit
    return Nothing
  SValInit t initExps -> do
    forEach initExps updateVarWithExp
    return Nothing
  SWhile e b -> do
    v <- eval e
    case v of
      IInteger 0 -> do return Nothing
      IInteger 1 -> do execBlock b
                       execStm s
  SIfElse e b1 b2 -> do
    v <- eval e
    case v of
      IInteger 0 -> do
        b <- execBlock b2
        return b
      _ -> do
        b <- execBlock b1
        return b
  SPrint e -> do
    v <- eval e
    output(showI v)
    return Nothing
  SFunc f -> do
    v <- setEntry f
    return Nothing
  SReturn e -> do
    v <- eval e
    return (Just v)
  SReturnE -> do
    return (Just IVoid)


setEntry :: Fun -> Action Void
setEntry f = case f of
  (Func t i p b) -> do
    updateFunc i f
    return ()

eval :: Exp -> Action InternalType
eval e = case e of
  EAssign i e -> do
    v <- eval e
    updateVar i v
    return v
  EDisjunction exp1 exp2 -> do
    v1 <- eval exp1
    case v1 of
      IInteger 1 -> do return v1
      IInteger 0 -> do v2 <- eval exp2
                       return v2
  EConjunction exp1 exp2 -> do
    v1 <- eval exp1
    case v1 of
      IInteger 0 -> do return v1
      IInteger 1 -> do v2 <- eval exp2
                       return v2
  EEq exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 == v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 == v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 == v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 == v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  ENeq exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 /= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 /= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 /= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 /= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  EGt exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 > v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 > v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 > v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 > v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  ELt exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 < v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 < v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 < v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 < v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  EGte exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 >= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 >= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 >= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 >= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  ELte exp1 exp2 -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do if v1 <= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IDouble vn2) -> do if v1 <= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IInteger vn1, IDouble vn2) -> do if v1 <= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
      (IDouble vn1, IInteger vn2) -> do if v1 <= v2
                                          then do return $IInteger 1
                                          else do return $IInteger 0
  EAdd exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do return $IInteger (vn1 + vn2)
      (IDouble vn1, IDouble vn2) -> do return $IDouble (vn1 + vn2)
      (IInteger vn1, IDouble vn2) -> do return $IDouble(fromInteger vn1 + vn2)
      (IDouble vn1, IInteger vn2) -> do return $IDouble (vn1 + fromInteger vn2)
      (IString vn1, IString vn2) -> do return $IString (vn1 ++ vn2)
  ESub exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do return $IInteger (vn1 - vn2)
      (IDouble vn1, IDouble vn2) -> do return $IDouble (vn1 - vn2)
      (IInteger vn1, IDouble vn2) -> do return $IDouble (fromInteger vn1 - vn2)
  EMul exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do return $IInteger (vn1 * vn2)
      (IDouble vn1, IDouble vn2) -> do return $IDouble (vn1 * vn2)
      (IInteger vn1, IDouble vn2) -> do return $IDouble (fromInteger vn1 * vn2)
  EDiv exp1 exp2  -> do
    v1 <- eval exp1
    v2 <- eval exp2
    case (v1,v2) of
      (IInteger vn1, IInteger vn2) -> do return $IInteger (vn1 `div` vn2)
      (IDouble vn1 , IDouble vn2) -> do return $IDouble(vn1 / vn2)
      (IInteger vn1, IDouble vn2) -> do
        return $IInteger(vn1 `div` round vn2)
      (IDouble vn1, IInteger vn2) -> do return $IDouble(vn1 / fromInteger vn2)
  ENeg e -> do
    v <- eval e
    case v of
      IInteger 0 -> do return $IInteger 1
      IInteger 1 -> do return $IInteger 0;
  EPreInc e -> do
    case e of
      EId i -> do
        env <- gets variables
        v <- lookupVar i env
        case v of
          IInteger vv ->
            let vn = vv + 1
            in do updateVar i $IInteger vn
                  return $IInteger vn
  EPostInc e -> do
    case e of
      EId i -> do
        env <- gets variables
        v <- lookupVar i env
        case v of
          IInteger vv ->
            let vn = vv + 1
            in do updateVar i $IInteger vn
                  return v
  EPreDec e -> do
    case e of
      EId i -> do
        env <- gets variables
        v <- lookupVar i env
        case v of
          IInteger vv ->
            let vn = vv - 1
            in do updateVar i $IInteger vn
                  return $IInteger vn
  EPostDec e -> do
    case e of
      EId i -> do
        env <- gets variables
        v <- lookupVar i env
        case v of
          IInteger vv ->
            let vn = vv - 1
            in do updateVar i $IInteger vn
                  return v
  EInt n -> do return $IInteger n
  EId  x -> do
    env <- gets variables
    v1 <- lookupVar x env
    return v1
  ETrue-> do return $IInteger 1
  EFalse-> do return $IInteger 0
  EDouble n -> do return $IDouble n
  EString n -> do return $IString n
  EFunc i exps -> do
    f <- lookupFunc i
    case f of
      (Func t i p b) -> do
        modify (\s -> s{variables = Map.empty : (variables s)})
        initNewVarEnv p exps
        v <- execBlock b
        modify (\s -> s{variables = tail(variables s)})
        case v of
          (Just vv) -> do return vv
          _ -> do return IVoid


-- Actions: functions with side effects on a state

-- an Action is a State monad on Environment
type Action a = State Env a

-- a familiar name for Action whose return value is uninteresting
type Void = ()

-- iterate over a list of elementes
forEach :: [x] -> (x -> Action Void) -> Action Void
forEach ss comp = mapM_ comp ss

-- the environment

data Env = ENV {
  variables :: [Map.Map Ident (Maybe InternalType)],
  functions :: Map.Map Ident Fun,
  outputs   :: [String]
  }

-- auxiliary functions

-- initial environment
initEnv :: Env
initEnv = ENV [Map.empty] Map.empty []

-- update the value of a variable
updateVar :: Ident -> InternalType -> Action Void
updateVar x v = modify (\s ->
  s{variables = (Map.insert x (Just v) (head (variables s))) : (tail (variables s))}
  )
updateFunc :: Ident -> Fun -> Action Void
updateFunc x f = modify (\s ->
  s{functions = Map.insert x f (functions s)}
  )
-- update with InitExp
updateVarWithExp :: Exp -> Action Void
updateVarWithExp ex = case ex of
  EAssign i e -> do x <- eval e
                    updateVar i x
  _ -> do return ()

updateVarInit :: Ident -> Action Void
updateVarInit x = modify (\s ->
  s{variables = (Map.insert x Nothing (head (variables s))) : (tail (variables s))}
  )
-- lookup the value of a variable
lookupVar :: Ident -> [(Map.Map Ident (Maybe InternalType))] -> Action InternalType
lookupVar key vars =
  case (key,vars) of
    (x,var1:vs) -> do
      case Map.lookup x var1 of
        Just (Just v) -> do return v
        Nothing -> do
          lookupVar x vs
    (x, []) -> do error ("unknown variable " ++ printTree x)

lookupFunc :: Ident -> Action Fun
lookupFunc x = do
  funcs <- gets functions
  case Map.lookup x funcs of
    Just v  -> return v
    Nothing -> error ("unknown function " ++ printTree x)

initNewVarEnv :: [Par] -> [Exp] -> Action Void
initNewVarEnv pars exps= do
  case pars of
    (Para t i):lpars -> do
      case exps of
         exp:lexps -> do
            v <- eval exp
            updateVar i v
            m <- initNewVarEnv lpars lexps
            return ()
    [] -> do return ()

-- generate output
output :: String -> Action Void
output m = modify (\s ->
  s{outputs = outputs s ++ [m]}
  )

showI :: InternalType -> String
showI i = case i of
  IInteger x -> show(x)
  IDouble x -> show(x)
  IString x -> show(x)
  _  -> show(i)


