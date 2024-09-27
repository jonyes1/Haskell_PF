module Main where

import Circulo ( diametro, area ) 


main :: IO()
main = do
    let raio = 3
    putStrLn $ "Diametro = " ++ show (diametro raio)
    putStrLn $ "Area = " ++ show (area raio)