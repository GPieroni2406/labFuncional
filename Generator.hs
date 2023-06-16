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

import Control.Monad.State

type Global = State Int

increment :: Global ()
increment = modify (+1)

decrement :: Global ()
decrement = modify (subtract 1)

getValue :: Global Int
getValue = get

setValue :: Int -> Global ()
setValue = put

runMyState :: Global a -> Int -> (a, Int)
runMyState = runState



-- CODE GENERATOR

genProgram :: Program -> String
genProgram Program defs main = "#include <stdio.h>"+ "\n" + convertirDefs defs + "\n" + convertirMain main

c :: Int
c = 0



convertirDefs :: Defs -> String
convertirDefs (x:xs) = convertirFuncion x xs ++ convertirDefs xs
convertirDefs [] = ""

-- Let, main y funciones precisan return 

convertirFuncion :: FunDef -> Defs -> String 
convertirFuncion (FunDef(a, x) ys e) defs = "int "+ convertirIdentificador a + "(" + separarLista ys + ")" + "{" + verificarLet e 0 + "return(" convertirExpr e 0 +"); };"


verificarLet :: Expr -> Integer -> String
verificarLet _ c = ""
verificarLet (Infix o e1 e2) c = verificarLet e1 c + verificarLet e2 c
verificarLet (If e1 e2 e3) c = verificarLet e1 c + verificarLet e2 c + verificarLet e3 c
verificarLet (Let (x,y) e1 e2) c = convertirLet (Let (x,y) (verificarLet e1 c) (verificarLet e2 c)) c
verificarLet (App n vs) c = map verificarLet vs



convertirExpr :: Expr -> Integer -> String 
convertirExpr (BoolLit x) c = convertirBool x
convertirExpr (IntLit x) c =  show x
convertirExpr (Var name) c = convertirIdentificador name
convertirExpr (Infix Eq e1 e2) c = (convertirExpr e1 c + "==" + convertirExpr e2 c)
convertirExpr (Infix NEq e1 e2) c = (convertirExpr e1 c + "!=" + convertirExpr e2 c)
convertirExpr (Infix GTh e1 e2) c = (convertirExpr e1 c + ">" + convertirExpr e2 c)
convertirExpr (Infix LTh e1 e2) c = (convertirExpr e1 c + "<" + convertirExpr e2 c)
convertirExpr (Infix GEq e1 e2) c = (convertirExpr e1 c + ">=" + convertirExpr e2 c)
convertirExpr (Infix LEq e1 e2) c = (convertirExpr e1 c + "<=" + convertirExpr e2 c)
convertirExpr (Infix Add e1 e2) c = (convertirExpr e1 c + "+" + convertirExpr e2 c)
convertirExpr (Infix Sub e1 e2) c = (convertirExpr e1 c + "-" + convertirExpr e2 c)
convertirExpr (Infix Mult e1 e2) c = (convertirExpr e1 c + "*" + convertirExpr e2 c)
convertirExpr (Infix Div e1 e2) c = (convertirExpr e1 c + "/" + convertirExpr e2 c)
convertirExpr (If e1 e2 e3) c = "If" + convertirExpr e1 c + "{" + convertirExpr e2 c + "}" + "else" + "{" + convertirExpr e3 c "};"
convertirExpr (Let (x,y) e1 e2) c = "_let" + show c + "("+ convertirExpr e1 c+1 +")"
convertirExpr (App n vs) c = show n + "(" + separarListaString (convertirEnApp vs c) + ")"


convertirEnApp :: [Expr] -> Integer -> [String]
convertirEnApp (v:vs) c = [convertirExpr v c] ++ [convertirEnApp vs c] 
convertirEnApp [] _ = []

convertirMain :: Expr -> String
convertirMain main = "int main {"
 + "\n"+ verificarLet main 0+ "\n" + "printf" + "(%d\n ," + convertirExpr main 0 + "); }"

convertirLet :: Expr -> Integer -> String
convertirLet (Let (x,y) e1 e2) c =  "int "+"_let" + show c  + "(int"+ convertirIdentificador x + ")" + "{"+"\n"+ verificarLet e1 c+1 +"\n"+"return (" + convertirExpr e2 c+1 + "); };"
convertirLet _ _ = ""

-------------------REVISAR PASAJE DE CONTADORES EN LETS------------------------

------------------Auxiliares--------------------------
separarListaString :: [String] -> String
separarListaString xs = intercalate "," xs

separarLista :: Show a => [a] -> String
separarLista xs = intercalate "," (map show xs)

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

