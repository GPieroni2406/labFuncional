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
checkProgram (Program xs exp)
  | length errores1 == 0 && length errores3 == 0 && length errores4 == 0 = Ok
  | length errores1 > 0 = Wrong errores1
  | length errores1 == 0 && length errores4 > 0 = Wrong errores4
 -- | length errores1 == 0 && length errores4 == 0 && length errores2 > 0 = Wrong errores2
  | length errores1 == 0 && length errores3 > 0 && length errores4 == 0 = Wrong errores3
  where
    errores1 = checkFunctionDup [] xs ++ checkParameter xs
    --errores2 = checkNameParameter xs ++ checkMain xs exp
    errores3 = compararVariablesFuncion xs ++ checkMain xs exp
    errores4 = checkTypeTotal xs exp

-------2.1----------------
checkFunctionDup::[String]->Defs->[Error]
checkFunctionDup ys (x:xs) | elem (obtenerNombre x) ys = [Duplicated (obtenerNombre x)] ++ checkFunctionDup ys xs
                           | otherwise = checkFunctionDup (obtenerNombre x :ys) xs 
checkFunctionDup ys [] = []


checkParameter::Defs->[Error]
checkParameter (x:xs) = checkParameterDup [] (obtenerParametros x) ++ checkParameter xs
checkParameter [] = []


checkParameterDup::[String]->[String]->[Error]
checkParameterDup xs (y:ys) | elem y xs =  [Duplicated y] ++ checkParameterDup xs (ys)
                            | otherwise = checkParameterDup (y:xs) ys
checkParameterDup xs [] = []

----------------------------------------------


-----------------2.2-------------------
checkNameParameter::Defs->[Error]
checkNameParameter (x:xs)| length (obtenerParametros x) == length (obtenerParametrosEnFirma x) = checkNameParameter xs
                        | otherwise = [ArgNumDef (obtenerNombre x) (length(obtenerParametrosEnFirma x)) (length(obtenerParametros x))] ++ checkNameParameter xs
checkNameParameter [] = []

checkNameParameterFunction::FunDef->[Error]
checkNameParameterFunction  (FunDef x y z)| length (obtenerParametros (FunDef x y z)) == length (obtenerParametrosEnFirma (FunDef x y z)) = []
                                      | otherwise = [ArgNumDef (obtenerNombre (FunDef x y z)) (length(obtenerParametrosEnFirma (FunDef x y z))) (length(obtenerParametros (FunDef x y z)))]

checkNameParameterApp::Expr -> Defs ->[Error]
checkNameParameterApp (App name vs) xs | (length vs) == (parametrosFunDef xs name) = []
                                       | otherwise = [ArgNumApp (name) (parametrosFunDef xs name) (length vs)]


checkMain :: Defs -> Expr -> [Error]
checkMain xs (App name ys) = (checkMainAux xs name (length ys)) ++ (checkMainColeccion xs ys) 
checkMain xs (Infix op e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs (If e1 e2 e3) = checkMain xs e1 ++ checkMain xs e2 ++ checkMain xs e3
checkMain xs (Let (x,y) e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs _ = []

checkMainAux:: Defs-> String-> Int -> [Error]
checkMainAux xs name cantidad | (parametrosFunDef xs name) == cantidad = []
                              | otherwise = [ArgNumApp (name) (parametrosFunDef xs name) (cantidad)]

-----------------------------------------

-------2.3-----------------------

compararVariablesFuncion::Defs->[Error]
compararVariablesFuncion (x:xs) = compararVariablesExpresion (obtenerParametros x) (obtenerExpresion x) ++ compararVariablesFuncion xs
compararVariablesFuncion [] = []

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

-------------------------------------------------------------------------------------------------------

-------------------------------------2.4----------------------------------
checkTypeTotal::Defs->Expr->[Error]
checkTypeTotal xs main = checkTypeDefinicion xs ++ checkTypeMain xs main

checkTypeMain:: Defs -> Expr ->[Error]
checkTypeMain xs (BoolLit x) = []
checkTypeMain xs (IntLit x) = []
checkTypeMain xs (Infix o e1 e2) | o `elem` [Eq,NEq,GEq,LEq,GTh,LTh] = corroborarTipo (obtenerTipoError [] e1 xs) [] e2 xs ++ corroborarTipo (obtenerTipoError [] e1 xs) [] e1 xs
checkTypeMain xs (Infix o e1 e2) | otherwise = corroborarTipo (TyInt) [] e2 xs ++ corroborarTipo (TyInt) [] e1 xs
checkTypeMain xs (App n vs) = corroborarTipo (getTipoFuncionPorNombre xs n) [] (App n vs) xs
checkTypeMain xs (If e1 e2 e3) = corroborarTipo TyBool [] e1 xs ++ corroborarTipo (obtenerTipoError [] e2 xs) [] e2 xs ++ corroborarTipo (obtenerTipoError [] e2 xs) [] e3 xs
checkTypeMain xs (Let (x,y) e1 e2) = corroborarTipo y [] e1 xs ++ corroborarTipo (obtenerTipoError [] e2 xs) [] e1 xs
checkTypeMain xs (Var name) = []

corroborarTipo :: Type -> [(Name,Type)] -> Expr -> Defs -> [Error]
corroborarTipo TyBool xs (IntLit x) ys = [Expected TyBool TyInt]

corroborarTipo TyInt xs (IntLit x) ys = []

corroborarTipo TyInt xs (BoolLit x) ys = [Expected TyInt TyBool]

corroborarTipo TyBool xs (BoolLit x) ys = []

corroborarTipo t xs  (Var x) ys | t == (obtenerTipoVariable x xs) = []
                                | otherwise = [Expected t (obtenerTipoVariable x xs)]
corroborarTipo t xs (Infix Eq e1 e2) ys |(TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix NEq e1 e2) ys| (TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix GTh e1 e2) ys| (TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix LTh e1 e2) ys| (TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix GEq e1 e2) ys| (TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix LEq e1 e2) ys| (TyBool == t) = corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
                                        | otherwise = [Expected t TyBool] ++ corroborarTipo (obtenerTipoError xs e1 ys) xs e1 ys ++  corroborarTipo (obtenerTipoError xs e1 ys) xs e2 ys
corroborarTipo t xs (Infix o e1 e2) ys | ((obtenerTipoOperador o) == t) = corroborarTipo (obtenerTipoOperador o) xs e1 ys ++ corroborarTipo (obtenerTipoOperador o) xs e2 ys
                                        | otherwise = [Expected t (obtenerTipoOperador o)]
corroborarTipo t xs  (If e1 e2 e3) ys = corroborarTipo (TyBool) xs e1 ys ++ corroborarTipo t xs e2 ys ++ corroborarTipo t xs e3 ys

corroborarTipo t xs  (Let (x,y) e1 e2) ys   | ((obtenerTipoError xs e2 ys) == t) = ((corroborarTipo y xs e1 ys) ++ (corroborarTipo t (sustituirDupla (x,(obtenerTipoError xs e2 ys)) xs) e2 ys))
                                            | otherwise = [Expected t (obtenerTipoError (sustituirDupla (x,(obtenerTipoError xs e2 ys)) xs) e2 ys)]
corroborarTipo t xs  (App name ws) ys | ((getTipoFuncionPorNombre ys name) == t) = checkNameParameterApp (App name ws) ys ++ verificarParametrosSegunFirma (getTiposFuncionPorNombre ys name) ws xs ys
                                      | otherwise = [Expected t (getTipoFuncionPorNombre ys name)] ++ verificarParametrosSegunFirma (getTiposFuncionPorNombre ys name) ws xs ys


-------------------------------------------------------------------------------------------------------


-----------------------------------------AUXILIARES---------------------------------------------

obtenerNombre::FunDef -> String
obtenerNombre (FunDef (n, x) ys e) = n

obtenerParametros::FunDef->[String]
obtenerParametros (FunDef(n, x) ys e) = ys

obtenerParametrosEnFirma :: FunDef->[Type]
obtenerParametrosEnFirma (FunDef(a, x) ys e) = listaEntrada x

listaEntrada::Sig->[Type]
listaEntrada (Sig xs y) = xs

obtenerExpresion::FunDef->Expr
obtenerExpresion (FunDef(a, x) ys e) = e


checkMainColeccion::Defs -> [Expr] -> [Error]
checkMainColeccion xs (y:ys) = checkMain xs y ++ checkMainColeccion xs ys
checkMainColeccion xs [] = []

parametrosFunDef::Defs->String->Int 
parametrosFunDef (x:xs) name | ((obtenerNombre x) == name) = length (obtenerParametrosEnFirma x)
                             | otherwise =  parametrosFunDef xs name

obtenerAmbiente::FunDef -> [(Name,Type)]
obtenerAmbiente  (FunDef (n,(Sig es s)) vs e) = zip vs es

checkTypeDefinicion :: Defs -> [Error]
checkTypeDefinicion (x:xs) = checkNameParameterFunction x ++ (corroborarTipo (obtenerTipoFuncion x) (obtenerAmbiente x) (obtenerExpresion x) (x:xs)) ++ checkTypeDefinicion xs
checkTypeDefinicion [] = []


verificarParametrosSegunFirma:: [Type] -> [Expr] ->[(Name,Type)]-> Defs -> [Error]
verificarParametrosSegunFirma (x:xs) (y:ys) zs ws = (corroborarTipo x zs y ws) ++ (verificarParametrosSegunFirma xs ys zs ws)
verificarParametrosSegunFirma (x:xs) [] zs ws = []
verificarParametrosSegunFirma [] _ _ _ = []


getTiposFuncionPorNombre :: Defs -> Name -> [Type]
getTiposFuncionPorNombre (x:xs) n | ((obtenerNombre x) == n) = obtenerParametrosEnFirma x
                                 | otherwise = getTiposFuncionPorNombre xs n


obtenerTipoOperador :: Op -> Type
obtenerTipoOperador o | o `elem` [Add,Sub,Mult,Div] = TyInt
                      | otherwise = TyBool
sustituirDupla :: (Name, Type)->[(Name, Type)] -> [(Name, Type)]
sustituirDupla (n,t) (x:xs)  = map (\x -> if obtenerName x == n then (n,t) else x) xs

obtenerTipoError :: [(Name,Type)]-> Expr -> Defs -> Type
obtenerTipoError xs (Var n) zs = obtenerTipoVariable n xs
obtenerTipoError xs (BoolLit _) zs = TyBool
obtenerTipoError xs (IntLit _) zs = TyInt
obtenerTipoError xs (Infix o e1 e2) zs | o `elem` [Eq, NEq] = obtenerTipoError xs e1 zs
                                       | otherwise = obtenerTipoOperador o
obtenerTipoError xs (If e1 e2 e3) zs = obtenerTipoError xs e2 zs
obtenerTipoError xs (App n ys) zs = getTipoFuncionPorNombre zs n
obtenerTipoError xs (Let (n, t) e1 e2) zs = obtenerTipoError xs e2 zs

obtenerTipoFuncion :: FunDef->Type
obtenerTipoFuncion (FunDef t ys e) = obtenerTipoRetorno t 

obtenerTipoRetorno :: TypedFun -> Type
obtenerTipoRetorno (_ , Sig _ x) = x

getTipoFuncionPorNombre :: Defs -> Name -> Type
getTipoFuncionPorNombre (x:xs) n | ((obtenerNombre x) == n) = obtenerTipoFuncion x 
                                 | otherwise = getTipoFuncionPorNombre xs n
obtenerName::(Name,Type) -> Name
obtenerName (a,b) = a

obtenerType::(Name,Type) -> Type
obtenerType (a,b) = b

obtenerTipoVariable::Name->[(Name,Type)] -> Type
obtenerTipoVariable x (y:ys) | x == (obtenerName y) = obtenerType y
                             | otherwise = obtenerTipoVariable x ys
obtenerTipoVariable x [] = TyBool                