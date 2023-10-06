module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Sección 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------


-- | Convierte un árbol LamTerm devuelto por el Parser en un término Term.
conversion :: LamTerm -> Term
-- Todas las variables se colocan como libres en un primer momento.
conversion (LVar varName) = (Free (Global varName))
-- Se aplica recursivamente la función a cada subarbol.
conversion (App lamTerm1 lamTerm2) = let t1 = (conversion lamTerm1)
                                         t2 = (conversion lamTerm2)
                                     in (t1 :@: t2)               
-- Convertimos el alcance de la abstracción a un término y luego ligamos las variables que coincidan con su variable de ligadura.
conversion (Abs varName lamTerm) = let termWOboundVars = (conversion lamTerm)
                                       termWboundVars = (boundVar varName 0 termWOboundVars)
                                   in (Lam termWboundVars)
            where
                boundVar :: String -> Int -> Term -> Term
                -- Se dejan las variables ligadas como están.
                boundVar _ _ bounded@(Bound n) = bounded
                -- El nombre de las variables libres se comparan con el nombre de la variable a ligar. Si es el mismo, la variable se liga, sino, se deja libre.
                boundVar varToBound n var@(Free (Global varName)) = if (varName == varToBound)
                                                                        then (Bound n)
                                                                        else var
                -- Se aplica la función recursivamente a los subtérminos de las aplicaciones.
                boundVar varToBound n (t1 :@: t2) = let t1' = (boundVar varToBound n t1)
                                                        t2' = (boundVar varToBound n t2)
                                                     in (t1' :@: t2')
                -- Se llama recursivamente sobre el alcance de una abstracción y aumenta la distancia n hasta la variable de ligadura
                boundVar varToBound n (Lam term) = let term' = (boundVar varToBound (n+1) term)
                                                   in (Lam term')

-------------------------------
-- Sección 3
-------------------------------

-- | Realiza la aplicación de valores
vapp :: Value -> Value -> Value
vapp (VLam f) v = (f v)
vapp (VNeutral  n) v = (VNeutral (NApp n v))

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
-- Para evaluar una variable ligada, se buscará el argumento pasado a su variable de ligadura cosrrespondiente en el entorno local.
eval' (Bound ii) (_, lEnv) = lEnv !! ii
-- Para las variables libres, buscamos en el contexto global su valor. En caso de no encontrarlo, se dejan como varibales libres.
eval' (Free (Global varName)) (gEnv, lEnv) = case (lookInGlobalState varName gEnv) of
                                                (Just value) -> value
                                                Nothing -> (VNeutral  (NFree (Global varName)))
eval' (Free (Quote n)) _ = (VNeutral  (NFree (Quote n)))
-- Se llama recursivamente sobre los subtérminos de una aplicación.
eval' (t1 :@: t2) env = let evalT1 = (eval' t1 env)
                            evalT2 = (eval' t2 env)
                        in vapp evalT1 evalT2
-- Devuelve una VLam que toma una función f :: Value -> Value.
    -- Al ser aplicada f sobre un Value, este se agregará al entorno local sobre el que será evaluado el término term que acompaña al primer argumento de eval' en este caso.
eval' (Lam term) (gEnv, lEnv) = (VLam (\y -> (eval' term (gEnv, y:lEnv))))

lookInGlobalState :: String -> (NameEnv Value) -> Maybe Value
lookInGlobalState _ [] = Nothing
lookInGlobalState var (((Global vName), value):xs) = if var == vName 
                                                        then (Just value)
                                                        else (lookInGlobalState var xs)
--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------
quote :: Value -> Term
quote val = (quote' val 0) 

quote' :: Value -> Int -> Term
-- Con el fin de poder imprimir abstracciones, aplicamos las funciones asociadas a un Value VLam a un Value "dummy".
quote' abs@(VLam f) n = Lam
                          (quote' 
                            (vapp
                               abs
                               (VNeutral (NFree (Quote n))))
                            (n +1))
quote' (VNeutral (NFree  g@(Global str))) n = (Free g)
quote' (VNeutral (NFree  (Quote m))) n = (Bound (n - m - 1))
quote' (VNeutral (NApp neu val)) n = (quote' (VNeutral neu) n) :@: (quote' val n)