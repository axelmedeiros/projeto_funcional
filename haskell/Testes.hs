module Tests where
import Main 
import Test.HUnit
import JsonParser
import Tipos



-- Use to especi
digits d n = fromInteger (round (d * 10^n)) / 10^n


testFilterYear01 = 
   TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do ano de 2017" 89731.57 (valor $ (head (getTransationsByYear  db 2017))))


testFilterYear02 =
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do ano de 2018" 43980.15 (valor $ (head (getTransationsByYear db 2018))))

testFilterYear03 =
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do ano de 2019" 131301.66 (valor $ (head (getTransationsByYear db 2019))))


testFilterYear04 = 
   TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do ano de 2017" (-2.9) (valor $ (last (getTransationsByYear  db 2017))))


testsFilterYear =
    TestList [ 
        TestLabel "Caso 2017" testFilterYear01, 
        TestLabel "Caso 2018" testFilterYear02,
        TestLabel "Caso 2019" testFilterYear03,
        TestLabel "Caso 2017 - Ultima transação" testFilterYear04 ]



testFilterMonth01 = 
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do mês em 07/2017" 29922.64 (valor $ (head (getTransationsByMounth db 2017 7))))


testFilterMonth02 = 
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do mês em 07/2017 - verificando a segunda" (-50.0) (valor $ ((getTransationsByMounth db 2017 7) !! 1)))


testFilterMonth03 = 
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do mês em 06/2018 - verificando o penultimo" (540.25) (valor $ (last ( init (getTransationsByMounth db 2018 6)))))


testFilterMonth04 = 
    TestCase (
    do 
        db <- getDB
        assertEqual "Filtrar transações do mês em 06/2018 - verificando o primeiro" (83900.33) (valor $ ( head (getTransationsByMounth db 2018 7))))


testsFilterMonth =
    TestList [ 
        TestLabel "Caso 1" testFilterMonth01, 
        TestLabel "Caso 2" testFilterMonth02,
        TestLabel "Caso 3" testFilterMonth03,
        TestLabel "Caso 4" testFilterMonth04 ]


testCreditsMonth01 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2018 8)
        assertEqual "Créditos do mês em 08/2018 - verificando a soma dos 3 primeiros" (4180.03) (calculateCreditsByMonth f1 2018 8))


testCreditsMonth02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2017 3)
        assertEqual "Créditos do mês em 08/2018 - verificando a soma dos 2 primeiros " (694.48) (calculateCreditsByMonth f1 2017 3))


testsCreditsMonth =
    TestList [ 
        TestLabel "Caso 1" testCreditsMonth01, 
        TestLabel "Caso 2" testCreditsMonth02]


testDebitsMonth01 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2017 11)
        assertEqual "Créditos de parte do mês em 11/2017" (-1681.03) (calculateDebtsByMonth f1 2017 11))


testDebitsMonth02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2018 1)
        assertEqual "Créditos de parte do mês em 12/2018" (-22147.67) (calculateDebtsByMonth f1 2018 1))



testsDebitsMonth =
    TestList [ 
        TestLabel "Caso 1" testDebitsMonth01, 
        TestLabel "Caso 2" testDebitsMonth02]


testRemainderMonth01 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 10 (getTransationsByMounth db 2019 4)
        assertEqual "Resto de parte do mês em 04/2019" (-50770.93) (calculateRemainderByMonth f1 2019 4))


testRemainderMonth02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2019 2)
        assertEqual "Resto de parte do mês em 02/2019" (4343.23) (calculateRemainderByMonth f1 2019 2))



testRemainderMonth03 = 
    TestCase (
    do 
        db <- getDB
        let f1 = getTransationsByMounth db 2019 5
        let debits = calculateDebtsByMonth db 2019 5
        let credits = calculateCreditsByMonth db 2019 5
        assertEqual "Resto do mês em 05/2019" (digits (debits + credits) 2) ( digits (calculateRemainderByMonth f1 2019 5) 2)
    )

testsRemainderMonth =
    TestList [ 
        TestLabel "Caso 1" testRemainderMonth01, 
        TestLabel "Caso 2" testRemainderMonth02,
        TestLabel "Caso 3" testRemainderMonth03 ]


testBalanceMonth01 =
    TestCase (
    do 
        db <- getDB
        let balanceNextMont = valor $ ( head (getTransationsByMounth db 2018 8))
        assertEqual "Saldo final de 07/2018" (balanceNextMont) (digits (calculateBalanceByMonth db 2018 7) 1))



testBalanceMonth02 =
    TestCase (
    do 
        db <- getDB
        let balanceNextMont = valor $ ( head (getTransationsByMounth db 2017 11))
        assertEqual "Saldo final de 10/2017" (balanceNextMont) (digits (calculateBalanceByMonth db 2017 10) 2))



testBalanceMonth03 =
    TestCase (
    do 
        db <- getDB
        let balanceNextMont = valor $ ( head (getTransationsByMounth db 2017 4))
        assertEqual "Saldo final de 03/2017" (balanceNextMont) (digits (calculateBalanceByMonth db 2017 3) 2))



testBalanceMonth04 =
    TestCase (
    do 
        db <- getDB
        let balanceNextMont = valor $ ( head (getTransationsByMounth db 2019 5))
        assertEqual "Saldo final de 04/2019 " (204249.14) (digits (calculateBalanceByMonth db 2019 4) 2))



testsBalanceMonth =
    TestList [ 
        TestLabel "Caso 1" testBalanceMonth01, 
        TestLabel "Caso 2" testBalanceMonth02,
        TestLabel "Caso 3" testBalanceMonth03,
        TestLabel "Caso 4" testBalanceMonth04 ]



testMaxBalanceMonth01 =
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2019 2)
        assertEqual "Saldo máximo de parte do mês em 02/2019" (162782.5) (calculateMaxBAbyMonth f1 2019 2))

testMaxBalanceMonth02 =
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2017 6)
        assertEqual "Saldo máximo de parte do mês em 02/2019" (25744.98) (calculateMaxBAbyMonth f1 2017 6))


testsMaxBalanceMonth = 
    TestList [ 
        TestLabel "Caso 1" testMaxBalanceMonth01, 
        TestLabel "Caso 2" testMaxBalanceMonth02 ]


testMinBalanceMonth01 =
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2019 2)
        assertEqual "Saldo mínimo de parte do mês em 02/2019" (156298.14) (digits (calculateMinBAbyMonth f1 2019 2) 2))



testMinBalanceMonth02 =
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2017 6)
        assertEqual "Saldo mínimo de parte do mês em 06/2017" (21290.88) (digits (calculateMinBAbyMonth f1 2017 6) 2))


testsMinBalanceMonth = 
    TestList [ 
        TestLabel "Caso 1" testMinBalanceMonth01, 
        TestLabel "Caso 2" testMinBalanceMonth02 ]


testCalculateAverageCreditsByYear01 = 
    TestCase (
    do 
        db <- getDB
        let f1 = filter (filterByValue (>) 0)(filter (filterByYearAndMonth 2018 0) db)
        assertEqual "Teste para média da receitas em 00/2018" (3498.386666666667) (calculateAverageCreditsByYear f1 2018))



testCalculateAverageCreditsByYear02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = filter (filterByValue (>) 0)(filter (filterByYearAndMonth 2019 0) db)
        assertEqual "Teste para média da receitas em 00/2019" (6256.8) (digits (calculateAverageCreditsByYear f1 2019) 2))


testsCalculateAverageCreditsByYear =
    TestList [ 
        TestLabel "Caso 1" testCalculateAverageCreditsByYear01, 
        TestLabel "Caso 2" testCalculateAverageCreditsByYear02 ]



testCalculateAverageDebitsByYear01 = 
    TestCase (
    do 
        db <- getDB
        let f1 = filter (filterByValue (<) 0)(filter (filterByYearAndMonth 2019 0) db)
        assertEqual "Teste para média das despesas em 02/2019" (-1299.24) (digits (calculateAverageDebitsByYear f1 2019) 2))



testCalculateAverageDebitsByYear02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = filter (filterByValue (<) 0)(filter (filterByYearAndMonth 2018 0) db)
        assertEqual "Teste para média das despesas em 02/2019" (-1333.93) (digits (calculateAverageDebitsByYear f1 2018) 2))

testsCalculateAverageDebitsByYear =
    TestList [ 
        TestLabel "Caso 1" testCalculateAverageDebitsByYear01, 
        TestLabel "Caso 2" testCalculateAverageDebitsByYear02 ]


testAverageRemainderByYear01 =
    TestCase (
    do 
        db <- getDB
        let f1 = (filter (filterByYearAndMonth 2019 4) db)
        assertEqual "Teste para média das sobras em 04/2019" (2620.64) (digits (calculateAverageRemainderByYear f1 2019) 2))


testAverageRemainderByYear02 =
    TestCase (
    do 
        db <- getDB
        let f1 = (filter (filterByYearAndMonth 2019 3) db)
        assertEqual "Teste para média das sobras em 02/2019" (952.74) (digits (calculateAverageRemainderByYear f1 2019) 2))



testsAverageRemainderByYear =
    TestList [ 
        TestLabel "Caso 1" testAverageRemainderByYear01, 
        TestLabel "Caso 2" testAverageRemainderByYear02 ]


testCashFlowMonth01 = 
    TestCase (
    do 
        db <- getDB
        let nextBalance = head (filter (filterByYearAndMonth 2018 3) db )
        assertEqual "Fluxo de caixa em 02/2018" (getValor nextBalance) ( digits (getV (last (cashFlowByMonth db 2018 2))) 2) 
    )


testCashFlowMonth02 = 
    TestCase (
    do 
        db <- getDB
        let nextBalance = head (filter (filterByYearAndMonth 2018 7) db )
        assertEqual "Fluxo de caixa em 02/2018" (getValor nextBalance) ( digits (getV (last (cashFlowByMonth db 2018 6))) 2) 
    )


testCashFlowMonth03 = 
    TestCase (
    do 
        db <- getDB
        let nextBalance = head (filter (filterByYearAndMonth 2017 5) db )
        assertEqual "Fluxo de caixa em 02/2018" (getValor nextBalance) ( digits (getV (last (cashFlowByMonth db 2017 4))) 2) 
    )


testsCashFlowMonth =
    TestList [ 
        TestLabel "Caso 1" testCashFlowMonth01, 
        TestLabel "Caso 2" testCashFlowMonth02,
        TestLabel "Caso 3" testCashFlowMonth03 ]




runAllTests =
    TestList [
        TestLabel "Filtro por ano" testsFilterYear,
        TestLabel "Filtro por mes" testsFilterMonth,
        TestLabel "Calcular as receitos por mes" testsCreditsMonth,
        TestLabel "Calcular as despesas por mes" testsDebitsMonth,
        TestLabel "Calcular as sobras por mes" testsRemainderMonth,
        TestLabel "Calcular o saldo final do mes" testsBalanceMonth,
        TestLabel "Calcular o saldo máximo do mes" testsMaxBalanceMonth,
        TestLabel "Calcular o saldo mínimo do mes" testsMaxBalanceMonth,
        TestLabel "Calcular média dos créditos por ano" testsCalculateAverageCreditsByYear,
        TestLabel "Calcular média das despesas por ano" testsCalculateAverageDebitsByYear,
        TestLabel "Calcular média das sobras por ano" testsAverageRemainderByYear,
        TestLabel "Calcular o fluxo de caixa por mês" testsCashFlowMonth

    ]


