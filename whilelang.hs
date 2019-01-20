module Whilelang where
import qualified Data.Map as M

data Expr = Val Integer | Vrb [Char] | Bin Op Expr Expr

data Op = Add | Sub | Mul | Lss | Grt

data State = Comp [State] | Assign Expr Expr | If Expr State State | While Expr State | Out Expr

instance Show Op where
    show Add = " + "
    show Sub = " - "
    show Mul = " * "
    show Grt = " > "
    show Lss = " < "

instance Show Expr where
    show (Vrb v) = v
    show (Val x) = show x
    show (Bin op x y) = "(" ++ show x ++ show op ++ show y ++ ")"

-- Pretty Printer
tab n = id(take n (repeat ' '))
pps n (Assign v e) = tab n ++ show v ++ " := " ++ show e ++ "\n"
pps n (If e st1 st2) = tab n ++ "if " ++ show e ++ " then\n" ++ pps (n+2) st1 ++ tab n ++ "else\n" ++ pps (n+2) st2 
pps n (While e st) = tab n ++ "while " ++ show e ++ " do\n" ++ pps (n+2) st
pps n (Out e) = tab n ++ "output " ++ show e ++ "\n"
pps n (Comp x) = tab n ++ "begin\n" ++ ppl (n+2) x
ppl n ([]) = tab(n-2) ++ "end\n"                    -- Print end of list
ppl n (head:tail) = pps n head ++ ppl n tail        -- List printer

-- Interpreter
type Table = M.Map [Char] Integer

eval (Vrb v) t = M.findWithDefault 0 v t
eval (Val x) _ = x
eval (Bin Add e1 e2) t = eval e1 t + eval e2 t
eval (Bin Sub e1 e2) t = eval e1 t - eval e2 t
eval (Bin Mul e1 e2) t = eval e1 t * eval e2 t
eval (Bin Grt e1 e2) t = if (eval e1 t > eval e2 t) then (1) else (0)
eval (Bin Lss e1 e2) t = if (eval e1 t < eval e2 t) then (1) else (0)

intp (Assign(Vrb v) e) t = M.insert v (eval e t) t
intp (If e st1 st2) t = if (eval e t == 1) then (intp st1 t) else (intp st2 t)
intp (While e st) t = if (eval e t == 0) then (t) else (intp (While e st) (intp st t))
intp (Comp []) t = t
intp (Comp (head : tail)) t = intp(Comp tail) (intp head t)

intpOut (Out v) t = putStrLn(show v ++ " = " ++ show(eval v t))

