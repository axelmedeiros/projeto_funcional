module Tests where
import Main 
import Test.HUnit
import JsonParser
import Tipos






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
        assertEqual "Créditos do mês em 11/2017" (-1681.03) (calculateDebtsByMonth f1 2017 11))


testDebitsMonth02 = 
    TestCase (
    do 
        db <- getDB
        let f1 = take 15 (getTransationsByMounth db 2018 1)
        assertEqual "Créditos do mês em 12/2018" (-22147.67) (calculateDebtsByMonth f1 2018 1))



testsDebitsMonth =
    TestList [ 
        TestLabel "Caso 1" testDebitsMonth01, 
        TestLabel "Caso 2" testDebitsMonth02]



