let database = null 

fetch("http://150.165.15.10:8080/todasTransacoes",{method: 'POST'})
    .then((response) => response.json())
    .then((json) => database = json.map(obj => new transaction(obj)))

// gera uma lista com intervalo fechado em start e aberto em end, seguindo um step
const range = (start, end, step=1) => {
    if(start > end) {
        return []
    } else {
        return [start].concat(range(start + step, end, step))
    }
    
}

// verifica se uma transacao é valida
const isValidTransaction = (transaction) => !transaction.tipos.includes(ENUM_transaction.APLICACAO) && !transaction.tipos.includes(ENUM_transaction.VALOR_APLICACAO);

// caputa as transacoes que não são SALDO_CORRENTE
const getTransactions = (monthTrasactions) => monthTrasactions.filter(transaction => !transaction.textoIdentificador == ENUM_transaction.SALDO_CORRENTE);

//soma no contexto da aplicação depende do resultado de uma função comparadora.
const sum = (total, transaction, comparator) => (comparator(transaction.valor)) ? total + transaction.valor : total

// filtra as transacoes por ano
const filterByYear = (database, year) => database.filter(transaction => transaction.data.ano == year);

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

//calcula a media anual das receitas
const calculateAnnualRevenueAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateRevenue(database, year, monthNumber), 0)) / 12;

//calcula a media anual das despesas
const calculateAnnualExpenseAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateExpense(database, year, monthNumber), 0)) / 12;

//calcula a media anual das sobras
const calculateAnnualLeftoversAverage = (database, year) => (range(0,11).reduce((total, monthNumber) => total + calculateLeftovers(database, year, monthNumber), 0)) / 12;
