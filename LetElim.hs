----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List


-- ELIMINACION DE LETs

letElimP :: Program -> Program 
letElimP (Program defs expr) = (Program (eliminarLetDef defs) (eliminarLetGeneral expr))

eliminarLetDef :: Defs -> Defs
eliminarLetDef [] = []
eliminarLetDef (x:xs) = [(eliminarLetFuncion x)] ++ (eliminarLetDef xs)

eliminarLetFuncion :: FunDef -> FunDef
eliminarLetFuncion (FunDef t ys (Infix op e1 e2)) = FunDef t ys (Infix op (eliminarLetGeneral e1) (eliminarLetGeneral e2))
eliminarLetFuncion (FunDef t ys (If e1 e2 e3)) = FunDef t ys (If (eliminarLetGeneral e1) (eliminarLetGeneral e2) (eliminarLetGeneral e3))
eliminarLetFuncion (FunDef t ys (Let (x, y) e1 e2)) = FunDef t ys (eliminarLetGeneral (Let (x, y) (eliminarLetGeneral e1) (eliminarLetGeneral e2)))
eliminarLetFuncion (FunDef t ys (App name xs)) = FunDef t ys (App name (map eliminarLetGeneral xs))
eliminarLetFuncion fd = fd



eliminarLetGeneral :: Expr -> Expr
eliminarLetGeneral (BoolLit x) = (BoolLit x)
eliminarLetGeneral (IntLit x) = (IntLit x)
eliminarLetGeneral (Var x) = (Var x)
eliminarLetGeneral (Infix op e1 e2) = (Infix op (eliminarLetGeneral e1) (eliminarLetGeneral e2)) 
eliminarLetGeneral (Let (x,y) e1 e2) = subst x (eliminarLetGeneral e1) (Let (x,y) (eliminarLetGeneral e1) (eliminarLetGeneral e2))
eliminarLetGeneral (App name xs) = ((App name (map eliminarLetGeneral xs)))   


subst :: Name -> Expr -> Expr -> Expr
subst n (BoolLit y) (Var x) | n == x = BoolLit y
                            | otherwise = Var x
subst n (IntLit y) (Var x) | n == x = IntLit y
                           | otherwise = Var x
subst n (IntLit y) (Infix op e1 e2) = Infix op (subst n (IntLit y) e1) (subst n (IntLit y) e2)
subst n (BoolLit y) (Infix op e1 e2) = Infix op (subst n (BoolLit y) e1) (subst n (BoolLit y) e2)


subst n (IntLit x) (If e1 e2 e3) = If (subst n (IntLit x) e1) (subst n (IntLit x) e2) (subst n (IntLit x) e3)
subst n (BoolLit x) (If e1 e2 e3) = If (subst n (BoolLit x) e1) (subst n (BoolLit x) e2) (subst n (BoolLit x) e3)

subst n (BoolLit z) (Let (x, y) e1 e2) | x == n = subst n (subst n (BoolLit z) e1) e2 
                                       | otherwise = Let (x, y) (subst n (BoolLit z) e1) (subst n (BoolLit z) e2)
subst n (IntLit z) (Let (x, y) e1 e2) | x == n = subst n (subst n (IntLit z) e1) e2 
                                      | otherwise = Let (x, y) (subst n (IntLit z) e1) (subst n (IntLit z) e2)

subst n e0 (App name xs) = App name (map (subst n e0) xs)
subst n e0 e = e  -- Catch-all pattern for any other expression

recorrerAppSubst :: Name -> Expr -> [Expr] -> [Expr]
recorrerAppSubst n e0 [] = []
recorrerAppSubst n e0 (x:xs) = [subst n e0 x] ++ recorrerAppSubst n e0 xs

