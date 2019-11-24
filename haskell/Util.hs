module Util
(
    exibeTabelaTransacoes,
    slice
) where

import Tipos

exibeTabelaTransacoes :: [Transacao] -> IO ()
exibeTabelaTransacoes transacoes = do 
    putStrLn cabecalho
    printLinha
    exibeTransacoes transacoes

exibeTransacoes :: [Transacao] -> IO ()
exibeTransacoes [] = printLinha
exibeTransacoes (x:xs) = do 
    putStrLn $ formataTransaction x
    exibeTransacoes xs

cabecalho =
    fmt 10 "   DATA" ++ " | " ++
    fmt 20 "   IDENTIFICADOR" ++ " | " ++
    fmt 12 "   VALOR" ++ " | " ++
    fmt 30 "          DESCRICAO" ++ " | " ++
    fmt 12 " NUMERO_DOC" ++ " | " ++
    fmt 20 "       TIPOS"

printLinha = putStrLn $ replicate 160 '-'

formataTransaction :: Transacao -> String
formataTransaction t =
    fmt 10 ( formataData $ datas t) ++ " | " ++
    fmt 20 (textoIdentificador t) ++ " | " ++
    fmt' 12 ( show $ valor t) ++ " | " ++
    fmt 30 ( descricao t) ++ " | " ++
    fmt' 12 ( numeroDOC t) ++ " | " ++
    ( show $ tipos t)

formataData :: GregorianCalendar -> String
formataData g = show (dayOfMonth g)++ "/"++ show (month g) ++"/"++ show (year g)

slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

fmt :: Int -> String  -> String
fmt esp str  
    | length str > esp = slice 0 (esp - 3) str ++ "..."
    | otherwise = str ++ replicate (esp - length str) ' '

fmt' :: Int -> String  -> String
fmt' esp str  
    | length str > esp = slice 0 (esp - 3) str ++ "..."
    | otherwise = replicate (esp - length str) ' ' ++ str 