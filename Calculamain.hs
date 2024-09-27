module Calculamain where

import Contas ( mult, dobro, quadr, triplo, raiz )

main :: IO ()
main = do
    putStrLn $ "A multiplicacao = " ++ show (mult 2 6)
    putStrLn $ "dobro = " ++ show (dobro 6)
    putStrLn $ "O quadrado = " ++ show (quadr 2)
    putStrLn $ "O triplo = " ++ show (triplo 2)
    putStrLn $ "A raiz = " ++ show (raiz 25)
