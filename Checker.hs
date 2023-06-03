----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


checkProgram :: Program -> Checked
checkProgram (Program xs exp) | (length hayError == 0) = Ok
                                | otherwise = Wrong (hayError)
                                where hayError = checkFunctionDup [] xs ++ checkParameter xs  ++ checkNameParameter xs  ++ compararVariablesFuncion xs ++ checkMain xs exp

checkFunctionDup::[String]->Defs->[Error]
--Recorro cada Funcion y verifico que no hay nombres repetidos.
checkFunctionDup ys (x:xs) | elem (obtenerNombre x) ys = [Duplicated (obtenerNombre x)] ++ checkFunctionDup ys xs
                           | otherwise = checkFunctionDup (obtenerNombre x :ys) xs 
checkFunctionDup ys [] = []

--Obtiene el nombre de la FunDef
obtenerNombre::FunDef -> String
obtenerNombre (FunDef (n, x) ys e) = n

obtenerParametros::FunDef->[String]
obtenerParametros (FunDef(n, x) ys e) = ys

obtenerParametrosEnFirma :: FunDef->[Type]
obtenerParametrosEnFirma (FunDef(a, x) ys e) = listaEntrada x

obtenerExpresion::FunDef->Expr
obtenerExpresion (FunDef(a, x) ys e) = e


listaEntrada::Sig->[Type]
listaEntrada (Sig xs y) = xs

--Mas General
checkParameter::Defs->[Error]
checkParameter (x:xs) = checkParameterDup [] (obtenerParametros x) ++ checkParameter xs
checkParameter [] = []

--Verifico que mi lista de parametros no tenga repetidos en la funcion
checkParameterDup::[String]->[String]->[Error]
checkParameterDup xs (y:ys) | elem y xs =  [Duplicated y] ++ checkParameterDup xs (ys)
                            | otherwise = checkParameterDup (y:xs) ys
checkParameterDup xs [] = []


checkNameParameter::Defs->[Error]
checkNameParameter (x:xs)| length (obtenerParametros x) == length (obtenerParametrosEnFirma x) = checkNameParameter xs
                        | otherwise = [ArgNumDef (obtenerNombre x) (length(obtenerParametrosEnFirma x)) (length(obtenerParametros x))] ++ checkNameParameter xs
checkNameParameter [] = []


compararVariablesFuncion::Defs->[Error]
compararVariablesFuncion (x:xs) = compararVariablesExpresion (obtenerParametros x) (obtenerExpresion x) ++ compararVariablesFuncion xs
compararVariablesFuncion [] = []

--Obtiene las variables de la expresion y se fija que todas esten en las variables de la izquierda.
compararVariablesExpresion :: [String]->Expr->[Error]
compararVariablesExpresion ys (Var x) | elem x ys = []
compararVariablesExpresion ys (Var x) | otherwise = [Undefined x]
compararVariablesExpresion ys (Infix op e1 e2) = compararVariablesExpresion ys e1 ++ compararVariablesExpresion ys e2
compararVariablesExpresion ys (If e1 e2 e3) = compararVariablesExpresion ys e1 ++ compararVariablesExpresion ys e2 ++ compararVariablesExpresion ys e3
compararVariablesExpresion ys (Let (x,y) e1 e2) = compararVariablesExpresion ys e1 ++ compararVariablesExpresion ys e2
compararVariablesExpresion ys (App name xs) = recorrerListaExpresion ys xs
compararVariablesExpresion ys _ = [] 
--Para el caso donde tenemos una lista de expresiones, la recorre y entra a la funcion de arriba.
recorrerListaExpresion :: [String]->[Expr]->[Error]
recorrerListaExpresion ys (x:xs) = compararVariablesExpresion ys x ++ recorrerListaExpresion ys xs
recorrerListaExpresion ys [] = []


checkMain :: Defs -> Expr -> [Error]
checkMain xs (App name ys) = (checkMainAux xs name (length ys)) ++ (checkMainColeccion xs ys) 
checkMain xs (Infix op e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs (If e1 e2 e3) = checkMain xs e1 ++ checkMain xs e2 ++ checkMain xs e3
checkMain xs (Let (x,y) e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs _ = []

checkMainColeccion::Defs -> [Expr] -> [Error]
checkMainColeccion xs (y:ys) = checkMain xs y ++ checkMainColeccion xs ys
checkMainColeccion xs [] = []


parametrosFunDef::Defs->String->Int 
parametrosFunDef (x:xs) name | ((obtenerNombre x) == name) = length (obtenerParametrosEnFirma x)
                             | otherwise =  parametrosFunDef xs name


checkMainAux:: Defs-> String-> Int -> [Error]
checkMainAux xs name cantidad | (parametrosFunDef xs name) == cantidad = []
                              | otherwise = [ArgNumApp (name) (parametrosFunDef xs name) (cantidad)]