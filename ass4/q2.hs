data Exp = Con Int -- integer constant
         | Var Var -- variable
         | Add Exp Exp -- e1 + e2
         | Mul Exp Exp -- e1 * e2
         | Sub Exp Exp -- e1 - e2
         | Div Exp Exp -- e1 `div` e2 (Integer divison)
         | Neg Exp -- (-e)
         | PP Var -- ++v
         | Assign Var Exp -- v = e

type Val = Int
type Var = String
type State = [(Var, Val)]

eval :: Exp -> State -> (Val, State)
eval (Con n) r = (n, r)
eval (Var x) r = case lookup x r of
                   Nothing -> error ("variable \"" ++ x ++ "\" is undefined.")
                   Just v -> (v, r)

eval (Add e1 e2) r = (fst c1 + fst c2, r ++ snd c1 ++ snd c2)
         where c1 = eval e1 r
               c2 = eval e2 (r ++ snd c1)

eval (Mul e1 e2) r = (fst c1 * fst c2, r ++ snd c1 ++ snd c2)
         where c1 = eval e1 r
               c2 = eval e2 (r ++ snd c1)

eval (Sub e1 e2) r = (fst c1 - fst c2, r ++ snd c1 ++ snd c2)
         where c1 = eval e1 r
               c2 = eval e2 (r ++ snd c1)

eval (Div e1 e2) r
  | fst (eval e2 r) == 0 = error ("Division by 0.")
  | otherwise = (fst c1 `div` fst c2, r ++ snd c1 ++ snd c2)
         where c1 = eval e1 r
               c2 = eval e2 (r ++ snd c1)

eval (Neg e) r = (negate (fst c), r ++ snd c)
         where c = eval e r

eval (PP v) r = case lookup v r of
                Nothing -> error ("variable \"" ++ v ++ "\" is undefined.")
                Just p -> (p+1, (v, p+1) : r)

eval (Assign v e) r = (fst c, (v, fst c) : r)
         where c = eval e r

exec :: Exp -> State -> State
exec e r = ("m", fst (eval e r)) : r

interpret :: Exp -> Int
interpret e = case lookup "m" $ exec e [] of
                Nothing -> 0
                Just v -> v
