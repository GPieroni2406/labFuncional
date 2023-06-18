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
genProgram (Program defs main) = "#include <stdio.h>"++  "\n" ++ convertirDefs defs ++ convertirMain main





convertirDefs :: Defs -> String
convertirDefs (x:xs) = convertirFuncion x xs ++ "\n" ++ convertirDefs xs
convertirDefs [] = ""

-- Let, main y funciones precisan return 

convertirFuncion :: FunDef -> Defs -> String 
convertirFuncion (FunDef(a, x) ys e) defs = "int "++ convertirIdentificador a ++ "(" ++ separarConComas ys ++ ")" ++ "{\n" ++ verificarLet e 0 ++ "return (" ++ convertirExpr e 0 ++"); };"


verificarLet :: Expr -> Integer -> String
verificarLet (Var x) c = ""
verificarLet (BoolLit x) c = ""
verificarLet (IntLit x) c = ""
verificarLet (Infix o e1 e2) c = verificarLet e1 c ++ verificarLet e2 c
verificarLet (If e1 e2 e3) c = verificarLet e1 c ++ verificarLet e2 c ++ verificarLet e3 c
verificarLet (Let (x,y) e1 e2) c = convertirLet (Let (x,y) e1 e2) c
verificarLet (App n vs) c = intercalate "\n" (verificarEnApp vs c)

convertirLet :: Expr -> Integer -> String
convertirLet (Let (x,y) e1 e2) c =  "int "++ "_let" ++ show c  ++ "(int "++  convertirIdentificador x ++ ")" ++ "{"++  verificarLet e1 (c+1) ++ "\n"++ "return (" ++ convertirExpr e2 (c+1) ++ "); };\n"
convertirLet _ _ = ""


convertirExpr :: Expr -> Integer -> String 
convertirExpr (BoolLit x) c = convertirBool (BoolLit x)
convertirExpr (IntLit x) c =  show x
convertirExpr (Var name) c = convertirIdentificador name
convertirExpr (Infix Eq e1 e2) c ="(" ++ (convertirExpr e1 c ++ " == " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix NEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ " != " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix GTh e1 e2) c = "(" ++ (convertirExpr e1 c ++ " > " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix LTh e1 e2) c = "(" ++ (convertirExpr e1 c ++ " < " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix GEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ " >= " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix LEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ " <= " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix Add e1 e2) c = "(" ++ (convertirExpr e1 c ++ " + " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix Sub e1 e2) c = "(" ++ (convertirExpr e1 c ++ " - " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix Mult e1 e2) c = "(" ++ (convertirExpr e1 c ++ " * " ++ convertirExpr e2 c) ++ ")"
convertirExpr (Infix Div e1 e2) c = "(" ++ (convertirExpr e1 c ++ " / " ++ convertirExpr e2 c) ++ ")"
convertirExpr (If e1 e2 e3) c = (convertirExpr e1 c) ++ "?" ++ (convertirExpr e2 c) ++ ":" ++ (convertirExpr e3 c)
convertirExpr (Let (x,y) e1 e2) c = "_let" ++ (show c) ++ "(" ++  (convertirExpr e1 (c+1)) ++ ")"
convertirExpr (App n vs) c = convertirIdentificador n ++ "(" ++ separarListaString (convertirEnApp vs c) ++ ")"

convertirEnApp :: [Expr] -> Integer -> [String]
convertirEnApp (v:vs) c = [convertirExpr v c] ++ convertirEnApp vs c
convertirEnApp [] _ = []


convertirMain :: Expr -> String
convertirMain main = "int main() {\n" ++ verificarLet main 0  ++ "printf(\"%d\\n\"," ++ convertirExpr main 0 ++ "); }"


-------------------REVISAR PASAJE DE CONTADORES EN LETS------------------------

------------------Auxiliares--------------------------
verificarEnApp :: [Expr] -> Integer -> [String]
verificarEnApp (v:vs) c | (verificarLet v c == "") = verificarEnApp vs c
                        | otherwise = [verificarLet v c] ++ verificarEnApp vs c
verificarEnApp [] _ = []

separarListaString :: [String] -> String
separarListaString xs = intercalate "," xs

separarConComas :: [String] -> String
separarConComas xs = intercalate "," (map (\x -> "int _" ++ x) xs)

convertirIdentificador :: Name -> String
convertirIdentificador name = "_" ++ name

convertirBool :: Expr -> String 
convertirBool (BoolLit True) = "1"
convertirBool (BoolLit False) = "0"
convertirBool _ = ""


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

