dobros :: [Int] -> [Int]                    --Assinatura de receber uma lista de numeros inteiro e retorna numeros inteiros 
dobros [] = []                              -- se a lista for vazia, retorna lista vazia
dobros (x:xs) = (2*x) : (dobros xs)         -- multiplica o primeiro numero da lista por dois depois retorna 
                                            -- o resto da lista que vai multiplicar por dois e asssim ate ao fim
                                            -- da linha que depois retorna a lista
                                    
impares :: [Int] -> [Bool]                  --Assinatura de receber uma lista de numeros inteiro e retorna verdadeiro ou falso
impares [] = []                             -- se a lista for vazia, retorna lista vazia
impares (x:xs) = (odd x) : (impares xs)     -- verifica se o primeiro numero da lista e impar depois retorna 
                                            -- o resto da lista que vai verificar se e impar, assim ate ao fim
                                            -- da linha que depois retorna a lista
                                            -- odd = impares 

pares :: [Int] -> [Bool]                    --Assinatura de receber uma lista de numeros inteiro e retorna verdadeiro ou falso
pares [] = []                               -- se a lista for vazia, retorna lista vazia
pares (x:xs) = (even x) : (pares xs)        -- verifica se o primeiro numero da lista e par depois retorna 
                                            -- o resto da lista que vai verificar se e par, assim ate ao fim
                                            -- da linha que depois retorna a lista
                                            -- even = pares 

maximos :: [(Float, Float)] -> [Float]      -- Recebe uma lista de tuplas e retorna lista de floats 
maximos [] = []                             -- se a lista for vazia , retorna vazia
maximos ((a,b):xs) = (max a b) : maximos xs -- verifica se a e maior que b depois retorna o maior 
                                            -- o resto da lista que vai verificar se a Ã© maior que b, assim ate ao fim
                                            -- da linha que depois retorna a lista
                                            -- max = maximo 

umaouduasletras :: [String] -> [String]     -- Recebe uma lista de strings e retorna uma lista de strings
umaouduasletras [] = []                     -- se for vazia retorna vazia
umaouduasletras (x:xs)                      -- Se o numero de caracteres da string for menor ou
    |length x <= 2 = x: (umaouduasletras xs)-- igual a 2, tira e coloca noutra lista
    |otherwise = umaouduasletras xs         -- se nao, vai a string a seguir, depois de correr a lista toda
                                            -- retorna lista com as palavras com menos de dois caracteres

filtra_impares :: [Int] -> [Int]            -- Recebe uma lista de inteiros e retorna uma lista de inteiros
filtra_impares [] = []                      -- Se for vazia, retorna uma lista vazia
filtra_impares (x:xs)                       -- Se os numeros forem impares, tira e coloca noutra lista
    |odd x = x: (filtra_impares xs)         -- se nao, passa ao numero seguinte, quando acabar a lista
    |otherwise = filtra_impares xs          -- retorna a lista com os impares

filtrapares :: [Int] -> [Int]               -- Recebe uma lista de inteiros e retorna uma lista de inteiros
filtrapares [] = []                         -- Se for vazia, retorna uma lista vazia
filtrapares (x:xs)                          -- Se os numeros forem impares, tira e coloca noutra lista
    |even x = x: (filtrapares xs)           -- se nao, passa ao numero seguinte, quando acabar a lista
    |otherwise = filtrapares xs             -- retorna a lista com os impares

paresord :: [(Int, Int)] -> [(Int, Int)]    --Recebe um lista de tuplas de inteiros e retorna uma lista de tuplas 
paresord [] = []                            --Se for vazia retorna vazia
paresord ((a, b): xs)                       --Se o primeiro elemento das tuplas for menor entao tira essa tupla da lista
    |a < b = (a,b) : paresord xs            -- Se nao, passa a proxima tupla, no fim retorna a lista em que o primeiro elemento e menor
    |otherwise = paresord xs

somaLista :: [Int] -> Int                   --Recebe uma lista de inteiros e retorna um inteiro
somaLista [] = 0                            -- Se a lista estiver vazia retorna 0
somaLista (x:xs) = x+(somaLista xs)         -- Soma todos os elementos da lista e retorna o resultado
                                            -- da  soma

multiLista :: [Int] -> Int                  --Recebe uma lista de inteiros e retorna um inteiro
multiLista [] = 1                           -- Se a lista estiver vazia retorna 0
multiLista (x:xs) = x*(multiLista xs)       -- Soma todos os elementos da lista e retorna o r

myconcat :: [String] -> String
myconcat [] = ""
myconcat (x:xs) = x++ myconcat xs