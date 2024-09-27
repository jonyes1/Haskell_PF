module Calc2Main where

import Calcula2 ( soma,somad,somaq ) 

main :: IO ()
main = do
  let x = 1 
  let y = 2
  let z = 3
  let s = soma x y z
  let sd = somad x y z
  let sq = somaq x y z
  putStrLn $ "Soma de tres numeros = " ++ show s
  putStrLn $ "Soma de 3 numeros dividido por 3 = " ++ show sd
  putStrLn $ "Quadrado da soma de tres numeros = " ++ show sq