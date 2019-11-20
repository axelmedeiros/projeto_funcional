// gera uma lista com intervalo fechado em start e aberto em end, seguindo um step
const range = (start, end, step=1) => {
    if(start > end) {
        return []
    } else {
        return [start].concat(range(start + step, end, step))
    }
    
}

// verifica se uma transacao é valida
const isValidTransaction = (transaction) => !transaction.tipos.includes(ENUM_TRANSACAO.APLICACAO) && !transaction.tipos.includes(ENUM_TRANSACAO.VALOR_APLICACAO);

// caputa as transacoes que não são SALDO_CORRENTE
const getTransactions = (monthTrasactions) => monthTrasactions.filter(transaction => {return transaction.textoIdentificador !== ENUM_TRANSACAO.SALDO_CORRENTE});

//soma no contexto da aplicação depende do resultado de uma função comparadora.
const sum = (total, transaction, comparator) => (comparator(transaction.valor)) ? total + transaction.valor : total

// filtra as transacoes por ano
const filterByYear = (database, year) => {console.log(database);

return    database.filter(transaction => transaction.data.ano == year)};

// filtra as transacoes por ano e mes
const filterByYearAndMonth = (database, year, month) => filterByYear(database, year).filter(transaction => transaction.data.mes == month);

// calcula a receita de um mês
const calculateRevenue = (database, year, month) => {
    const monthTrasactions = filterByYearAndMonth(database, year, month)
    const validTransactions = getTransactions(monthTrasactions).filter(transaction => isValidTransaction(transaction))
    return validTransactions.reduce((total, transaction) => sum(total, transaction, (value) => value > 0), 0)
}

// calcula a despesa de um mês
const calculateExpense = (database, year, month) => {
    const monthTrasactions = filterByYearAndMonth(database, year, month)
    const validTransactions = getTransactions(monthTrasactions).filter(transaction => isValidTransaction(transaction))
    return Math.abs(validTransactions.reduce((total, transaction) => sum(total, transaction, (value) => value < 0), 0))
}

// calcula as sobras de um mês
const calculateLeftovers = (database, year, month) => calculateRevenue(database, year, month) - calculateExpense(database, year, month);

// calcula o saldo 
const calculateBalanceByYearAndMonth = (database, year, month) => {
    const validTransactions = getTransactions(filterByYearAndMonth(database, year, month))
    .filter(transaction => isValidTransaction(transaction))
    const response = validTransactions.reduce((total, t) => total + t.valor ,0)
    return response
}


//calcula a media anual das receitas
const calculateAnnualRevenueAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateRevenue(database, year, monthNumber), 0)) / 12;

//calcula a media anual das despesas
const calculateAnnualExpenseAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateExpense(database, year, monthNumber), 0)) / 12;

//calcula a media anual das sobras
const calculateAnnualLeftoversAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateLeftovers(database, year, monthNumber), 0)) / 12;


const functionsCarrier = {
    "filterByYear" : filterByYear,
    "filterByYearAndMonth": filterByYearAndMonth,
    "calculateRevenue": calculateRevenue,
    "calculateExpense": calculateExpense,
    "calculateLeftovers": calculateLeftovers,
    "calculateBalanceByYearAndMonth": calculateBalanceByYearAndMonth,
    "calculateAnnualRevenueAverage": calculateAnnualRevenueAverage,
    "calculateAnnualExpenseAverage": calculateAnnualExpenseAverage,
    "calculateAnnualLeftoversAverage": calculateAnnualLeftoversAverage
}