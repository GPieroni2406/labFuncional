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
verificarLet (Var x) _ = ""
verificarLet (BoolLit x) _ = ""
verificarLet (IntLit x) _ = ""
verificarLet (Infix o e1 e2) c = verificarLet e1 c ++ verificarLet e2 (obtenerContador (convertirLet e1 c))
verificarLet (If e1 e2 e3) c = verificarLet e1 c ++ verificarLet e2 (obtenerContador (convertirLet e1 c)) ++ verificarLet e3 (obtenerContador (convertirLet e2 (obtenerContador (convertirLet e1 c))))
verificarLet (Let (x,y) e1 e2) c = verificarLet e1 c ++ obtenerString (convertirLet (Let (x,y) e1 e2) (obtenerContador (convertirLet e1 c)))
verificarLet (App n vs) c = intercalate "\n" (verificarEnApp vs c)

convertirLet :: Expr -> Integer -> (String,Integer)
convertirLet (Let (x,y) e1 e2) c = ("int "++ "_let" ++ show k  ++ "(int "++  convertirIdentificador x ++ ")" ++ "{\n"++ verificarLet e2 c ++ "return (" ++ convertirExpr e2 c ++ "); };\n" , k+1)
                                where 
                                    k =(obtenerContador (convertirLet e2 c))
convertirLet (Infix o e1 e2) c = ((obtenerString (convertirLet e1 c)) ++ (obtenerString (convertirLet e2 k)), (obtenerContador (convertirLet e2 k)))
                            where
                                k = (obtenerContador (convertirLet e1 c))
convertirLet (If e1 e2 e3) c = ((obtenerString (convertirLet e1 c)) ++ (obtenerString (convertirLet e2 k)) ++ (obtenerString (convertirLet e3 j)) ,(obtenerContador (convertirLet e3 j)))
                            where
                                k = (obtenerContador (convertirLet e1 c))
                                j = (obtenerContador (convertirLet e2 k))
convertirLet (App n vs) c = (anidar (map fst (recorrerAppLets vs c)), sum (map snd (recorrerAppLets vs c)))
convertirLet _ c = ("",c)

anidar :: [String] -> String
anidar (v:vs) = v ++ anidar vs
anidar [] = ""


recorrerAppLets :: [Expr]-> Integer -> [(String,Integer)]
recorrerAppLets (v:vs) c = [(convertirLet v c)] ++ recorrerAppLets vs (obtenerContador (convertirLet v c))
recorrerAppLets [] c = [("",c)]

obtenerContador :: (String,Integer) -> Integer
obtenerContador (_,c) = c

obtenerString :: (String,Integer) -> String 
obtenerString (x,_) = x


convertirExpr :: Expr -> Integer -> String 
convertirExpr (BoolLit x) c = convertirBool (BoolLit x)
convertirExpr (IntLit x) c =  show x
convertirExpr (Var name) c = convertirIdentificador name
convertirExpr (Infix Eq e1 e2) c ="(" ++ (convertirExpr e1 c ++ "==" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix NEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ "!=" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix GTh e1 e2) c = "(" ++ (convertirExpr e1 c ++ ">" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix LTh e1 e2) c = "(" ++ (convertirExpr e1 c ++ "<" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix GEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ ">=" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix LEq e1 e2) c = "(" ++ (convertirExpr e1 c ++ "<=" ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix Add e1 e2) c = "(" ++ (convertirExpr e1 c ++ " + " ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix Sub e1 e2) c = "(" ++ (convertirExpr e1 c ++ " - " ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix Mult e1 e2) c = "(" ++ (convertirExpr e1 c ++ " * " ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (Infix Div e1 e2) c = "(" ++ (convertirExpr e1 c ++ " / " ++ convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ")"
convertirExpr (If e1 e2 e3) c = (convertirExpr e1 c) ++ "?" ++ (convertirExpr e2 (obtenerContador (convertirLet e1 c))) ++ ":" ++ (convertirExpr e3 (obtenerContador (convertirLet e2 (obtenerContador (convertirLet e1 c)))))
convertirExpr (Let (x,y) e1 e2) c = "_let" ++ (show j) ++ "(" ++  (convertirExpr e1 c) ++ ")"
                                where 
                                    k = (obtenerContador (convertirLet e1 c))
                                    j = (obtenerContador (convertirLet e2 k))
convertirExpr (App n vs) c = convertirIdentificador n ++ "(" ++ separarListaString (convertirEnApp vs c) ++ ")"

convertirEnApp :: [Expr] -> Integer -> [String]
convertirEnApp (v:vs) c = [convertirExpr v c] ++ convertirEnApp vs c
convertirEnApp [] _ = []



convertirMain :: Expr -> String
convertirMain main = "int main() {\n" ++ verificarLet main 0  ++ "printf(\"%d\\n\"," ++ convertirExpr main 0 ++ "); }" ++ "\n"


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

