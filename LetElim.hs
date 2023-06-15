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
eliminarLetGeneral (Let (x,y) e1 e2) = subst x (eliminarLetGeneral e1) (eliminarLetGeneral e2)
eliminarLetGeneral (App name xs) = ((App name (map eliminarLetGeneral xs)))   


subst :: Name -> Expr -> Expr -> Expr
subst n e0 (BoolLit x) = (BoolLit x)
subst n e0 (IntLit x) = (IntLit x)
subst n e0 (Var x) | (n==x) = e0
                   | otherwise = (Var x)
subst n e0 (Infix op e1 e2) = (Infix op (subst n e0 e1) (subst n e0 e2))
subst n e0 (If e1 e2 e3) = (If (subst n e0 e1) (subst n e0 e2) (subst n e0 e3))
subst n e0 (Let (x,y) e1 e2) | (x==n) = subst n (subst n e0 e1) e2 
                             | otherwise = (Let (x,y) (subst n e0 e1) (subst n e0 e2))  
subst n e0 (App name xs) = (App name (recorrerAppSubst n e0 xs))




recorrerAppSubst :: Name -> Expr -> [Expr] -> [Expr]
recorrerAppSubst n e0 [] = []
recorrerAppSubst n e0 (x:xs) = [subst n e0 x] ++ recorrerAppSubst n e0 xs

