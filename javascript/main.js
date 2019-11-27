// gera uma lista com intervalo fechado em start e aberto em end, seguindo um step
const range = (start, end, step=1) => {
    if(start > end) {
        return []
    } else {
        return [start].concat(range(start + step, end, step))
    }
    
}

// verifica se uma transacao é valida
const isValidTransaction = (transaction) => !transaction.tipos.includes("APLICACAO") && !transaction.tipos.includes("VALOR_APLICACAO");

// captura as transacoes que não são SALDO_CORRENTE
const getTransactions = (transactions) => transactions.filter(transaction => !transaction.tipos.includes("SALDO_CORRENTE"));

// recupera as transcoes que nao
const getValidTransactions = (database, year) => {
    const monthTrasactions = filterByYear(database, year)
    const validTransactions = getTransactions(monthTrasactions).filter(transaction => isValidTransaction(transaction))
    return validTransactions
}


const validTransactionsOnMonth = (database, year, month) => getValidTransactions(database, year).filter(t=> t.data.mes===month)

//soma no contexto da aplicação depende do resultado de uma função comparadora.
const sum = (total, transaction, comparator) => (comparator(transaction.valor)) ? total + transaction.valor : total

// filtra as transacoes por ano
const filterByYear = (database, year) => database.filter(transaction => transaction.data.ano == year);

// filtra as transacoes por ano e mes
const filterByYearAndMonth = (database, year, month) => filterByYear(database, year).filter(transaction => transaction.data.mes == month);

// calcula a receita de um mês
const calculateRevenue = (database, year, month) => {
    const validTransactions = validTransactionsOnMonth(database, year, month)
    return validTransactions.reduce((total, transaction) => sum(total, transaction, (value) => value > 0), 0)
}

// calcula a despesa de um mês
const calculateExpense = (database, year, month) => {
    const validTransactions = validTransactionsOnMonth(database, year, month)
    return Math.abs(validTransactions.reduce((total, transaction) => sum(total, transaction, (value) => value < 0), 0))
}

// calcula as sobras de um mês
const calculateLeftovers = (database, year, month) => calculateRevenue(database, year, month) - calculateExpense(database, year, month);

// calcula o saldo 
const calculateBalanceByYearAndMonth = (database, year, month) => {
    const validTransactions = filterByYearAndMonth(database, year, month).filter(transaction => isValidTransaction(transaction))
    const response = validTransactions.reduce((total, t) => total + t.valor ,0)
    return response
}

function aux(array, func) {
    if(array.length == 2) {
        return func(array[0].valor, array[1].valor)
    } else {
        const head = array[0]
        array.shift() 
        return func(head.valor, head.valor + aux(array, func))
    }

}

const calculeBalaceMaxAchieved = (database, year, month) => {
    const validTransactions = filterByYearAndMonth(database, year, month).filter(transaction => isValidTransaction(transaction))
    const response = aux(validTransactions, Math.max)
    return response
}

const calculeBalaceMinAchieved = (database, year, month) => {
    const validTransactions = filterByYearAndMonth(database, year, month).filter(transaction => isValidTransaction(transaction))
    const response = aux(validTransactions, Math.min)
    return Math.abs(response)
}

//calcula a media anual das receitas
const calculateAnnualRevenueAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateRevenue(database, year, monthNumber), 0)) / getValidTransactions(database, year).filter(t=>t.valor>0).length;

//calcula a media anual das despesas
const calculateAnnualExpenseAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateExpense(database, year, monthNumber), 0)) / getValidTransactions(database, year).filter(t=>t.valor<0).length;

//calcula a media anual das sobras
const calculateAnnualLeftoversAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateLeftovers(database, year, monthNumber), 0)) / 12;


// pega o fluxo do caixa                ---------------->considerando que todos os meses tem 31 dias
const getCashFlow = (database, year, month) => {
    const validTransactions = filterByYearAndMonth(database, year, month).filter(transaction => isValidTransaction(transaction))
    const sortedTransactionsByDay = validTransactions.sort((t1, t2) => t1.data.diaDoMes - t2.data.diaDoMes)
    return getCashFlowAux(sortedTransactionsByDay, 1, 0)
}

const getCashFlowAux = (transacoes,  day, prev) => {
    let dayBalance = 0
    if(day <= 31){
        const dayTransactions = transacoes.filter(t => t.data.diaDoMes === day);

        if(dayTransactions.length !== 0)
            dayBalance = dayTransactions.reduce((total, t) => total + t.valor, 0) 
        
        return [{"day":day, "dayBalance": dayBalance + prev}].concat(getCashFlowAux(transacoes, day + 1, dayBalance + prev))
    } else {
        return []
    }
} 

const functionsCarrier = {
    "filterByYear" : filterByYear,
    "filterByYearAndMonth": filterByYearAndMonth,
    "calculateRevenue": calculateRevenue,
    "calculateExpense": calculateExpense,
    "calculateLeftovers": calculateLeftovers,
    "calculeBalaceMaxAchieved": calculeBalaceMaxAchieved,
    "calculeBalaceMinAchieved": calculeBalaceMinAchieved,
    "calculateBalanceByYearAndMonth": calculateBalanceByYearAndMonth,
    "calculateAnnualRevenueAverage": calculateAnnualRevenueAverage,
    "calculateAnnualExpenseAverage": calculateAnnualExpenseAverage,
    "calculateAnnualLeftoversAverage": calculateAnnualLeftoversAverage,
    "getCashFlow": getCashFlow
}