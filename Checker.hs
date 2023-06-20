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
checkProgram (Program xs exp) = if (length (checkFunctionDup [] xs ++ checkParameter xs)) > 0
                                then Wrong (checkFunctionDup [] xs ++ checkParameter xs)
                                else if (length (compararVariablesFuncion xs ++ verificarFuncionesExistentes xs exp [])) > 0 
                                     then Wrong (compararVariablesFuncion xs ++ verificarFuncionesExistentes xs exp []) 
                                     else if (length (checkTypeTotal xs exp)) > 0 
                                          then Wrong (checkTypeTotal xs exp) 
                                          else Ok


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
checkMain xs (App name ys) = (checkMainColeccion xs ys) 
checkMain xs (Infix op e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs (If e1 e2 e3) = checkMain xs e1 ++ checkMain xs e2 ++ checkMain xs e3
checkMain xs (Let (x,y) e1 e2) = checkMain xs e1 ++ checkMain xs e2
checkMain xs _ = []

checkMainColeccion::Defs -> [Expr] -> [Error]
checkMainColeccion xs (y:ys) = checkMain xs y ++ checkMainColeccion xs ys
checkMainColeccion xs [] = []


checkMainAux:: Defs-> String-> Int -> [Error]
checkMainAux xs name cantidad | (parametrosFunDef xs name) == cantidad = []
                              | otherwise = [ArgNumApp (name) (parametrosFunDef xs name) (cantidad)] 

-----------------------------------------

-------2.3-----------------------

compararVariablesFuncion :: Defs -> [Error]
compararVariablesFuncion defs = compararVariablesFuncionAux defs defs


compararVariablesExpresion :: [String]->Expr->Defs->[Error]
compararVariablesExpresion ys (Var x) zs | elem x ys = []
                                         | otherwise = [Undefined x]
compararVariablesExpresion ys (Infix op e1 e2) zs = compararVariablesExpresion ys e1 zs ++ compararVariablesExpresion ys e2 zs
compararVariablesExpresion ys (If e1 e2 e3) zs = compararVariablesExpresion ys e1 zs ++ compararVariablesExpresion ys e2 zs ++ compararVariablesExpresion ys e3 zs
compararVariablesExpresion ys (Let (x,y) e1 e2) zs | (elem x ys) = compararVariablesExpresion ys e1 zs ++ compararVariablesExpresion ys e2 zs
                                                   | otherwise = compararVariablesExpresion (x:ys) e1 zs ++ compararVariablesExpresion (x:ys) e2 zs
compararVariablesExpresion ys (App name xs) zs | (existeFunDef zs name) =  recorrerListaExpresion ys xs zs
                                               | otherwise = [Undefined name] ++ recorrerListaExpresion ys xs zs
compararVariablesExpresion ys _ zs = [] 
--Para el caso donde tenemos una lista de expresiones, la recorre y entra a la funcion de arriba.
recorrerListaExpresion :: [String]->[Expr]-> Defs ->[Error]
recorrerListaExpresion ys (x:xs) zs = compararVariablesExpresion ys x zs ++ recorrerListaExpresion ys xs zs
recorrerListaExpresion ys [] zs = []

verificarFuncionesExistentes :: Defs -> Expr -> [(Name,Type)] -> [Error]
verificarFuncionesExistentes xs (App name ys) zs | (existeFunDef xs name) = desglosarExpresiones xs ys zs
                                                 | otherwise = [Undefined name] ++ desglosarExpresiones xs ys zs
verificarFuncionesExistentes xs (Infix op e1 e2) zs = verificarFuncionesExistentes xs e1 zs ++ verificarFuncionesExistentes xs e2 zs
verificarFuncionesExistentes xs (If e1 e2 e3) zs = verificarFuncionesExistentes xs e1 zs ++ verificarFuncionesExistentes xs e2 zs ++ verificarFuncionesExistentes xs e3 zs
verificarFuncionesExistentes xs (Let (x,y) e1 e2) zs = verificarFuncionesExistentes xs e1 zs ++ verificarFuncionesExistentes xs e2 ([(x,y)] ++ zs)
verificarFuncionesExistentes xs (Var x) zs | (consultarVariableAmbiente x zs) = []
                                           | otherwise = [Undefined x] 
verificarFuncionesExistentes xs _ zs = []



desglosarExpresiones :: Defs -> [Expr] -> [(Name,Type)]-> [Error]
desglosarExpresiones xs (y:ys) zs = verificarFuncionesExistentes xs y zs ++ desglosarExpresiones xs ys zs
desglosarExpresiones xs [] _ = []

-------------------------------------------------------------------------------------------------------

-------------------------------------2.4----------------------------------
checkTypeTotal :: Defs -> Expr -> [Error]
checkTypeTotal defs main = checkTypeTotalAux defs defs main

checkTypeMain:: Defs -> Expr ->[Error]
checkTypeMain xs (BoolLit x) = []
checkTypeMain xs (IntLit x) = []
checkTypeMain xs (Infix o e1 e2) | o `elem` [Eq,NEq,GEq,LEq,GTh,LTh] = corroborarTipo (obtenerTipoError [] e1 xs) [] e1 xs ++ corroborarTipo (obtenerTipoError [] e1 xs) [] e2 xs
                                 | otherwise = corroborarTipo (TyInt) [] e1 xs ++ corroborarTipo (TyInt) [] e2 xs
checkTypeMain xs (App n vs) = corroborarTipo (getTipoFuncionPorNombre xs n) [] (App n vs) xs
checkTypeMain xs (If e1 e2 e3) = corroborarTipo TyBool [] e1 xs ++ corroborarTipo (obtenerTipoError [] e2 xs) [] e2 xs ++ corroborarTipo (obtenerTipoError [] e2 xs) [] e3 xs
checkTypeMain xs (Let (x,y) e1 e2) = corroborarTipo y [] e1 xs ++ corroborarTipo (obtenerTipoError (sustituirDupla (x,y) []) e2 xs) [(x,y)] e2 xs
checkTypeMain xs (Var name) = []

corroborarTipo :: Type -> [(Name,Type)] -> Expr -> Defs -> [Error]

corroborarTipo TyInt xs (IntLit x) ys = []

corroborarTipo TyInt xs (BoolLit x) ys = [Expected TyInt TyBool]

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
corroborarTipo t xs  (If e1 e2 e3) ys = corroborarTipo (TyBool) xs e1 ys ++ corroborarTipo t xs e2 ys ++ corroborarTipo (obtenerTipoError xs e2 ys) xs e3 ys
--corroborarTipo t xs  (If e1 e2 e3) ys = corroborarTipo t xs e2 ys ++ corroborarTipo (obtenerTipoError xs e2 ys) xs e3 ys ++ corroborarTipo (TyBool) xs e1 ys 

corroborarTipo TyBool xs (IntLit x) ys = [Expected TyBool TyInt]

corroborarTipo t xs  (App name ws) ys | ((getTipoFuncionPorNombre ys name) == t) = checkNameParameterApp (App name ws) ys ++ verificarParametrosSegunFirma (getTiposFuncionPorNombre ys name) ws xs ys
                                      | otherwise = [Expected t (getTipoFuncionPorNombre ys name)] ++ checkNameParameterApp (App name ws) ys  ++ verificarParametrosSegunFirma (getTiposFuncionPorNombre ys name) ws xs ys

corroborarTipo TyBool xs (BoolLit x) ys = []

corroborarTipo t xs  (Let (x,y) e1 e2) ys   | ((obtenerTipoError (sustituirDupla (x,y) xs) e2 ys) == t) = ((corroborarTipo y xs e1 ys) ++ (corroborarTipo (obtenerTipoError (sustituirDupla (x,y) xs) e2 ys) (sustituirDupla (x,y) xs) e2 ys))
                                            | otherwise = [Expected t (obtenerTipoError (sustituirDupla (x,y) xs) e2 ys)] ++ ((corroborarTipo y xs e1 ys) ++ (corroborarTipo (obtenerTipoError (sustituirDupla (x,y) xs) e2 ys) (sustituirDupla (x,y) xs) e2 ys))


-------------------------------------------------------------------------------------------------------


-----------------------------------------AUXILIARES---------------------------------------------
compararVariablesFuncionAux :: Defs -> Defs -> [Error]
compararVariablesFuncionAux (x:xs) ys = compararVariablesExpresion (obtenerParametros x) (obtenerExpresion x) ys ++ compararVariablesFuncionAux xs ys
compararVariablesFuncionAux [] _ = []


checkTypeTotalAux :: Defs -> Defs -> Expr -> [Error]
checkTypeTotalAux xs ys main = checkTypeDefinicion xs ++ checkTypeMain xs main

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

parametrosFunDef :: Defs -> String -> Int
parametrosFunDef (x:xs) name
  | obtenerNombre x == name = length (obtenerParametrosEnFirma x)
  | otherwise = parametrosFunDef xs name
parametrosFunDef [] _ = 0

existeFunDef :: Defs -> String -> Bool
existeFunDef (x:xs) name | (obtenerNombre x == name) = True
                         | otherwise = existeFunDef xs name
existeFunDef [] _ = False

obtenerAmbiente::FunDef -> [(Name,Type)]
obtenerAmbiente  (FunDef (n,(Sig es s)) vs e) = zip vs es

checkTypeDefinicion :: Defs -> [Error]
checkTypeDefinicion defs = checkTypeDefinicionAux defs defs

checkTypeDefinicionAux :: Defs -> Defs -> [Error]
checkTypeDefinicionAux (x:xs) ys
  | length errores > 0 = errores ++ checkTypeDefinicionAux xs ys
  | otherwise = corroborarTipo (obtenerTipoFuncion x) (obtenerAmbiente x) (obtenerExpresion x) ys ++ checkTypeDefinicionAux xs ys
  where errores = checkNameParameterFunction x
checkTypeDefinicionAux [] ys = []



verificarParametrosSegunFirma:: [Type] -> [Expr] ->[(Name,Type)]-> Defs -> [Error]
verificarParametrosSegunFirma (x:xs) (y:ys) zs ws = (corroborarTipo x zs y ws) ++ (verificarParametrosSegunFirma xs ys zs ws)
verificarParametrosSegunFirma _ [] zs ws = []
verificarParametrosSegunFirma [] _ zs ws = []


getTiposFuncionPorNombre :: Defs -> Name -> [Type]
getTiposFuncionPorNombre (x:xs) n | ((obtenerNombre x) == n) = obtenerParametrosEnFirma x
                                 | otherwise = getTiposFuncionPorNombre xs n


obtenerTipoOperador :: Op -> Type
obtenerTipoOperador o | o `elem` [Add,Sub,Mult,Div] = TyInt
                      | otherwise = TyBool

sustituirDupla :: (Name, Type) -> [(Name, Type)] -> [(Name, Type)]
sustituirDupla (n,t) (x:xs)
  | elem (n,t) (map (\x -> if obtenerName x == n then (n,t) else x) (x:xs)) = map (\x -> if obtenerName x == n then (n,t) else x) (x:xs)
  | otherwise = (n,t) : map (\x -> if obtenerName x == n then (n,t) else x) (x:xs)
sustituirDupla (n,t) [] = [(n,t)]

obtenerTipoError :: [(Name,Type)]-> Expr -> Defs -> Type
obtenerTipoError xs (Var n) zs = obtenerTipoVariable n xs
obtenerTipoError xs (BoolLit _) zs = TyBool
obtenerTipoError xs (IntLit _) zs = TyInt
obtenerTipoError xs (Infix o e1 e2) zs | elem o [Eq, NEq,GTh,LEq,GEq,LTh] = TyBool
                                       | otherwise = TyInt
obtenerTipoError xs (If e1 e2 e3) zs = obtenerTipoError xs e2 zs
obtenerTipoError xs (App n ys) zs = getTipoFuncionPorNombre zs n
obtenerTipoError xs (Let (n, t) e1 e2) zs = obtenerTipoError (sustituirDupla (n,t) xs) e2 zs


obtenerTipoFuncion :: FunDef->Type
obtenerTipoFuncion (FunDef t ys e) = obtenerTipoRetorno t 

obtenerTipoRetorno :: TypedFun -> Type
obtenerTipoRetorno (_ , Sig _ x) = x

getTipoFuncionPorNombre :: Defs -> Name -> Type
getTipoFuncionPorNombre (x:xs) n
  | obtenerNombre x == n = obtenerTipoFuncion x
  | otherwise = getTipoFuncionPorNombre xs n
getTipoFuncionPorNombre [] _ = error "Function not found."


obtenerName::(Name,Type) -> Name
obtenerName (a,b) = a

obtenerType::(Name,Type) -> Type
obtenerType (a,b) = b
               
obtenerTipoVariable :: Name -> [(Name, Type)] -> Type
obtenerTipoVariable x xs =
  case lookup x xs of
    Just t -> t
    Nothing -> error "La variable no se encontró en la lista."

consultarVariableAmbiente::Name->[(Name,Type)] -> Bool

consultarVariableAmbiente x (y:ys) | x == (obtenerName y) = True
                             | otherwise = consultarVariableAmbiente x ys
consultarVariableAmbiente x [] = False           