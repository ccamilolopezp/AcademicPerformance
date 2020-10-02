-- Este modulo contiene funciones para calcular el desempeño de un estudiante en una materia, trabaja solo dos casos:
-- 1. Cuando en una materia todas las tareas / trabajos tienen el mismo peso. 
-- 2. Cuando en una materia las tareas / trabajos tienen diferente peso.
module Desempeño where

-- Calcula La longitud de una lista.
longitudLista :: [Float] -> Float
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs

-- Calcula la suma ponderada de una lista, con respecto a otra donde se espera vengan los pesos de cada nota.
-- Se asume que ambas listas vienen con igual longitud para usar esta función.
sumaPonderada :: [Float] -> [Float] -> Float
sumaPonderada [] [] = 0
sumaPonderada (x:xs) (y:ys) = x * y + sumaPonderada xs ys

-- Calcula la suma de todos los elementos de una lista.
acumularLista :: [Float] -> Float
acumularLista [] = 0
acumularLista (x:xs) = x + acumularLista xs

--Calcula el promedio de una lista.
promedioUsualLista :: [Float] -> Float
promedioUsualLista[] = 0
promedioUsualLista x = acumularLista x / longitudLista x 

--Calcula el promedio ponderado de una lista.
promedioPonderadoLista :: [Float] -> [Float] -> Float
promedioPonderadoLista [] [] = 0
promedioPonderadoLista x y = sumaPonderada x y / acumularLista y 

--Calcula el desempeño de una lista de notas donde todas tienen igual valor
desempenoUsual :: [Float] -> String
desempenoUsual [] = "bajo"
desempenoUsual x = if promedioUsualLista x < 3  then "bajo"
                   else if promedioUsualLista x < 3.5 then "basico"
                   else if promedioUsualLista x < 4.5 then "medio"
                   else "alto"   

--Calcula el desempeño de una lista de notas con respecto a otra en la que se esperan vengan los pesos de cada nota,
--Se asume que ambas listas vienen con igual longitud.
desempenoPonderado :: [Float] -> [Float] -> String
desempenoPonderado [] [] = "bajo"
desempenoPonderado x y = if promedioPonderadoLista x y < 3  then "bajo"
                         else if promedioPonderadoLista x y < 3.5 then "basico"
                         else if promedioPonderadoLista x y < 4.5 then "medio"
                         else "alto"     