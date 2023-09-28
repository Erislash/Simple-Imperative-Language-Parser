module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int



-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case (M.lookup v s) of
                Just n -> (Right n)
                Nothing -> (Left UndefVar)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n s = (M.insert v n s)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip       s  = Right (Skip :!: s)
stepComm (Let v e)  s  = case (evalIntexp e s) of
                         (Right (n :!: s')) -> Right (Skip :!: (update v n s'))
                         (Left  err) -> (Left  err)

stepComm (Seq Skip c1) s = Right (c1 :!: s)
stepComm (Seq c0   c1) s = case (stepComm c0 s) of
                            Right (c0' :!: s') -> Right ((Seq c0' c1) :!: s')
                            Left err -> Left err

stepComm (IfThenElse b c0 c1) s = case (evalBoolExp b s) of
                                   Right (e :!: s') -> if (e)
                                                        then Right (c0 :!: s')
                                                        else Right (c1 :!: s')
                                   Left err -> Left err

stepComm (Repeat c cond) s = Right (Seq c (IfThenElse cond Skip (Repeat c cond)) :!: s)


-- Evalua una expresion
-- Completar la definición
evalIntexp :: Exp Int -> State -> Either Error (Pair Int State)
evalIntexp (Const n) s = Right (n :!: s)
evalIntexp (Var var) s = case (lookfor var s) of
                         (Right n) -> Right (n :!: s)
                         (Left  UndefVar) -> (Left  UndefVar) 
evalIntexp (UMinus e) s = case (evalIntexp e s) of
                          (Right (e' :!: s')) -> Right (-e' :!: s')
                          (Left  err) -> (Left  err)
evalIntexp (Plus a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' + b') :!: s'')
                                                 (Left err) -> Left err
                          (Left err) -> Left err

evalIntexp (Minus a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' - b') :!: s'')
                                                 (Left err) -> Left err
                          (Left err) -> Left err

evalIntexp (Times a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' * b') :!: s'')
                                                 (Left err) -> Left err
                          (Left err) -> Left err

evalIntexp (Div a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> if b' == 0 then (Left DivByZero) else Right ((a' `div` b') :!: s'')
                                                 (Left err) -> (Left err)
                          (Left err) -> (Left err)

evalIntexp (EAssgn var e) s = case (evalIntexp e s) of
                               Right (n :!: s') -> Right (n :!: (update var n s'))
                               (Left err) -> (Left err)

evalIntexp (ESeq c1 c2) s = case (evalIntexp c1 s) of
                            Right (_ :!: s') -> case (evalIntexp c2 s') of
                                                  Right (e' :!: s'') -> Right (e' :!: s'')
                                                  (Left err) -> Left err
                            (Left err) -> Left err

evalBoolExp :: Exp Bool -> State -> Either Error (Pair Bool State)
evalBoolExp BTrue s = Right (True :!: s)
evalBoolExp BFalse s = Right (False :!: s)
evalBoolExp (Lt a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' < b') :!: s'')
                                                 (Left err) -> Left err
                          (Left err) -> Left err



evalBoolExp (Gt a b) s = case (evalIntexp a s) of
                          (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' > b') :!: s'')
                                                 (Left err) -> Left err
                          (Left err) -> Left err

evalBoolExp (And a b) s = case (evalBoolExp a s) of
                              (Right (a' :!: s')) -> case (evalBoolExp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' && b') :!: s'')
                                                 (Left err) -> Left err
                              (Left err) -> Left err

evalBoolExp (Or a b) s = case (evalBoolExp a s) of
                              (Right (a' :!: s')) -> case (evalBoolExp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' || b') :!: s'')
                                                 (Left err) -> Left err
                              (Left err) -> Left err

evalBoolExp (Not a) s = case (evalBoolExp a s) of
                          Right (a' :!: s') -> Right ((not a') :!: s')
                          Left err -> Left err

evalBoolExp (Eq a b) s = case (evalIntexp a s) of
                              (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' == b') :!: s'')
                                                 (Left err) -> Left err
                              (Left err) -> Left err

evalBoolExp (NEq a b) s = case (evalIntexp a s) of
                              (Right (a' :!: s')) -> case (evalIntexp b s') of
                                                 (Right (b' :!: s'')) -> Right ((a' /= b') :!: s'')
                                                 (Left err) -> Left err
                              (Left err) -> Left err