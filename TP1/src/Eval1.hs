module Eval1
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
lookfor :: Variable -> State -> Int
lookfor v s = case (M.lookup v s) of
                Just n -> n
                Nothing -> error"Intento de acceso a variable inexistente"

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n s = (M.insert v n s)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición

stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s)
stepComm (Let v e) s = let (n :!: s') = (evalIntexp e s)
                       in (Skip :!: (update v n s'))

stepComm (Seq Skip c1) s =  (c1 :!: s)
stepComm (Seq c0   c1) s = let (c0' :!: s') = stepComm c0 s
                           in ((Seq c0' c1) :!: s')

stepComm (IfThenElse cond tBranch fBranch) s = let (e :!: s') = (evalBoolExp cond s)
                                                in if (e)
                                                    then (tBranch :!: s')
                                                    else (fBranch :!: s')
stepComm (Repeat c cond) s = (Seq c (IfThenElse cond Skip (Repeat c cond)) :!: s)


-- Evalua una expresion
-- Completar la definición

evalIntexp :: Exp Int -> State -> Pair Int State
evalIntexp (Const n) s = (n :!: s)
evalIntexp (Var var) s = ((lookfor var s) :!: s)
evalIntexp (UMinus e) s = let (e' :!: s') = evalIntexp e s
                          in (-e' :!: s')
evalIntexp (Plus a b) s = let (a' :!: s') = evalIntexp a s
                              (b' :!: s'') = evalIntexp b s'
                          in ((a' + b') :!: s'')
evalIntexp (Minus a b) s = let (a' :!: s') = evalIntexp a s
                               (b' :!: s'') = evalIntexp b s'
                           in ((a' - b') :!: s'')
evalIntexp (Times a b) s = let (a' :!: s') = evalIntexp a s
                               (b' :!: s'') = evalIntexp b s'
                           in ((a' * b') :!: s'')
evalIntexp (Div a b) s = let (a' :!: s') = evalIntexp a s
                             (b' :!: s'') = evalIntexp b s'
                         in ((a' `div` b') :!: s'')

evalIntexp (EAssgn var e) s = let (e' :!: s') = (evalIntexp e s)
                              in (e' :!: (update var e' s'))
evalIntexp (ESeq c1 c2) s = let (_ :!: s') = evalIntexp c1 s
                                (e' :!: s'') = evalIntexp c2 s'
                            in (e' :!: s'')


evalBoolExp :: Exp Bool -> State -> Pair Bool State
evalBoolExp BTrue s = (True :!: s)
evalBoolExp BFalse s = (False :!: s)
evalBoolExp (Lt e1 e2) s = let  (e1' :!: s') = evalIntexp e1 s
                                (e2':!: s'') = evalIntexp e2 s'
                            in ((e1' < e2') :!: s'')
evalBoolExp (Gt e1 e2) s = let  (e1' :!: s') = evalIntexp e1 s
                                (e2':!: s'') = evalIntexp e2 s'
                            in ((e1' > e2') :!: s'')
evalBoolExp (And e1 e2) s = let   (e1' :!: s') = evalBoolExp e1 s
                                  (e2' :!: s'') = evalBoolExp e2 s'
                            in ((e1' && e2') :!: s'')
evalBoolExp (Or e1 e2) s = let  (e1' :!: s') = evalBoolExp e1 s
                                (e2' :!: s'') = evalBoolExp e2 s'
                           in ((e1' || e2') :!: s'')
evalBoolExp (Not e1) s = let   (e1' :!: s') = evalBoolExp e1 s
                          in ((not e1') :!: s')
evalBoolExp (Eq e1 e2) s = let  (e1' :!: s') = evalIntexp e1 s
                                (e2' :!: s'') = evalIntexp e2 s'
                           in ((e1' == e2') :!: s'')
evalBoolExp (NEq e1 e2) s = let   (e1' :!: s') = evalIntexp e1 s
                                  (e2' :!: s'') = evalIntexp e2 s'
                            in ((e1' /= e2') :!: s'')
