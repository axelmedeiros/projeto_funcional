module Main where
import JsonParser
import Tipos
import Util

main :: IO()
main = do
    putStrLn "Fim."


getYear (GregorianCalendar y _ _ ) = y
getMonth (GregorianCalendar _ m _ ) = m
getDayOfMonth (GregorianCalendar _ _ d) = d

getValor (Transacao _ _ v _ _ _  _) = v
getDatas (Transacao d _ _ _ _ _ _ ) = d
getType (Transacao _ _ _ _ _ _ t ) = t




-- Filtrar transações por ano.

filterByYear y (Transacao d _ _ _ _ _ tip) = y == getYear d && not (elem "APLICACAO" tip) && not (elem "VALOR_APLICACAO" tip)

filterTransationsByYear y (Transacao d _ _ _ _ _ tip) = y == getYear d
getTransationsByYear _data y = filter (filterTransationsByYear y) _data

-- Filtrar transações por ano e mês.
filterByYearAndMonth y m (Transacao d _ _ _ _ _ tip) = y == getYear d && m == getMonth d && not (elem "APLICACAO" tip) && not (elem "VALOR_APLICACAO" tip)

filterTransationsByMonth y m (Transacao d _ _ _ _ _ tip) = y == getYear d && m == getMonth d
getTransationsByMounth _data y m = filter (filterTransationsByMonth y m) _data


-- Calcular o valor das receitas (créditos) em um determinado mês e ano.

filterByValue f value (Transacao _ _ v _ _ _ tipos) = f v value && not (elem "SALDO_CORRENTE" tipos)
calculateCreditsByMonth _data y m = foldr (+) 0 (map (getValor) f2) 
    where
        f1 = filter (filterByYearAndMonth y m) _data
        f2 = filter (filterByValue (>=) 0) f1

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.


calculateDebtsByMonth _data y m = foldr (+) 0 (map (getValor) f2) 
    where
        f1 = filter (filterByYearAndMonth y m) _data
        f2 = filter (filterByValue (<=) 0) f1


-- Calcular a sobra (receitas - despesas) de determinado mês e ano

filterTransations (Transacao _ _ _ _ _ _ tipos) = not (elem "SALDO_CORRENTE" tipos)
calculateRemainderByMonth _data y m = foldr (+) 0 (map (getValor) f2) 
    where
        f1 = filter (filterByYearAndMonth y m) _data
        f2 = filter (filterTransations) f1


-- Calcular o saldo final em um determinado ano e mês

calculateBalanceByMonth _data y m = foldr (+) 0 (map (getValor) f1) 
    where
        f1 = filter (filterByYearAndMonth y m) _data



-- Calcular o saldo máximo atingido em determinado ano e mês 
-- TODO: Posso jogar tudo em uma lista

calculateBalaceAchieved [] b = [b] 
calculateBalaceAchieved (x:xs) balance
    = [balance] ++ calculateBalaceAchieved xs newBalance
    where
        newBalance = (getValor x) + balance

calculateMaxBAbyMonth [] _ _ = error "A função necessita de transações "
calculateMaxBAbyMonth _data y m =  maximum (calculateBalaceAchieved (tail f1) balance) 
    where
        f1 = filter (filterByYearAndMonth y m) _data
        balance = getValor (head f1)



-- Calcular o saldo mínimo atingido em determinado ano e mês
-- TODO: Posso jogar tudo em uma lista

calculateMinBAbyMonth [] _ _ = error "A função necessita de transações "
calculateMinBAbyMonth _data y m = minimum (calculateBalaceAchieved (tail f1) balance) 
    where
        f1 = filter (filterByYearAndMonth y m) _data
        balance = getValor (head f1)



-- Calcular a média das receitas em determinado ano

calculateAverageCreditsByYear _data y = (foldr (+) 0 (map (getValor) f2)) / fromIntegral (length f2) 
    where
        f1 = filter (filterByYear y) _data 
        f2 = filter (filterByValue (>) 0) f1



-- Calcular a média das despesas em determinado ano
calculateAverageDebitsByYear _data y = (foldr (+) 0 (map (getValor) f2)) / fromIntegral (length f2) 
    where
        f1 = filter (filterByYear y) _data 
        f2 = filter (filterByValue (<) 0) f1


-- Calcular a média das sobras em determinado ano

calculateAverageRemainderByYear _data y = (foldr (+) 0 (map (getValor) f2)) / 12
    where
        f1 = filter (filterByYear y) _data 
        f2 = filter (filterTransations) f1

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista 
-- contendo pares (dia,saldoFinalDoDia).



getD (d, _) = d
getV (_, v) = v

calculateBalanceByDay [] balance d = [(d, balance)]
calculateBalanceByDay (x:xs) balance d 
    | dayT == d = calculateBalanceByDay xs newBalance d
    | otherwise = [(d, balance)] ++ calculateBalanceByDay xs newBalance dayT
    where
        dayT = getD x
        valueT = getV x
        newBalance = balance + valueT 

cashFlowByMonth _data y m = calculateBalanceByDay (tail f2) balance day
    where
        f1 = filter (filterByYearAndMonth y m) _data
        getDay t = getDayOfMonth (getDatas t)
        f2 = [ (getDay t, getValor t) | t <- f1 ]
        balance = getV (head f2)
        day = getD (head f2)


