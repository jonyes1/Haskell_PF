module Circulo where


square :: Double -> Double 
square x = x * x

diametro :: Double -> Double
diametro x = x * 2

area :: Double -> Double 
area x = pi * square(x)