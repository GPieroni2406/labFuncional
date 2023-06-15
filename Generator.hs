----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List

-- CODE GENERATOR

genProgram :: Program -> String
genProgram Program defs main = "#include <stdio.h>"+ "\n" + convertirDefs defs + "\n" + convertirMain main

convertirDefs :: Defs -> String
convertirDefs (x:xs) = convertirFuncion x xs ++ convertirDefs xs
convertirDefs [] = ""

convertirFuncion :: FunDef -> Defs -> String 
convertirFuncion (FunDef(a, x) ys e) defs = "int" + convertirIdentificador a + 

convertirMain :: Expr -> String
convertirMain main = "int main {" -- Se debe poner en let en caso de que haya
 + "\n" + "printf" + "(%d\n ," + convertirExp main + ");}"



------------------Auxiliares--------------------------
convertir

convertirIdentificador :: Name -> String
convertirIdentificador name = "_" + show name

convertirBool :: BoolLit -> String 
convertirBool (BoolLit True) = "1"
convertirBool (BoolLit False) = "0"

convertirBool :: TyBool -> String 
convertirBool True = "1"
convertirBool False = "0"

obtenerNombre::FunDef -> String
obtenerNombre (FunDef (n, x) ys e) = n

obtenerParametros::FunDef->[String]
obtenerParametros (FunDef(n, x) ys e) = ys

obtenerParametrosEnFirma :: FunDef->[Type]
obtenerParametrosEnFirma (FunDef(a, x) ys e) = listaEntrada x

listaEntrada::Sig->[Type]
listaEntrada (Sig xs y) = xs

tipoSalida :: Sig -> Type
tipoSalida (Sig xs y) = y

