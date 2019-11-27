const ENUM_TRANSACAO = {
    SALDO_CORRENTE : "Saldo Corrente", 
    VALOR_APLICACAO : "Valor aplicado",
    RECEITA_OPERACIONAL : "Receita Operacional", 
    TAXA_CONDOMINIO : "Taxas de Condominio",
    TAXA_EXTRA : "Taxas Extras",
    TAXA_SALAO_FESTA : "Taxas Salao de Festas",
    MULTAS_JUROS : "Multas e Juros",
    TAXA_AGUA : "Taxa de Agua",
    RECUPERACAO_ATIVOS  : "Recuperacao de Ativos",
    MULTA_JURO_CORRECAO_COBRANCA : "Multa, Juros e Correcao de Cobranca",
    OUTRAS_RECEITAS : "Outras receitas",
    
    DESPESAS_PESSOAL : "Despesas com pessoal",
    TERCEIRIZACAO_FUNCIONARIOS : "Terceirizacao de Funcionarios",
    VIGILANCIA : "Vigilancia",
    SALARIO_FUNCIONARIOS_ORGANICOS : "Salario dos funcionarios organicos",
    ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS: "Adiantamento salarial dos funcionarios organicos",

    FERIAS : "Ferias",
    INSS : "INSS funcionarios e vigilancia",

    FGTS : "FGTS",
    PIS : "PIS",
    ISS : "ISS",
    BENEFICIO_SOCIAL : "Beneficio social dos funcionarios organicos",
    OUTRAS_DESPESAS_PESSOAL : "Outras despesas com pessoal",
    
    DESPESAS_ADMINISTRATIVAS : "Despesas Administrativas",
    ENERGISA : "Energisa",
    CAGEPA : "CAGEPA",
    COMPRA : "Compra",
    ADMINISTRACAO_CONDOMINIO : "Administracao do condominio",
    MANUTENCAO : "Manutencao realizada",
    ABASTECIMENTO : "Abastecimentos",
    SERVICOS_TERCEIROS : "Servicos realizados por terceiros",
    IRPF : "Imposto de renda recolhido na fonte",
    TARIFAS_BANCARIAS : "Tarifas e taxas bancarias",
    OUTRAS_DESPESAS_ADMINISTRATIVAS : "Outras despesas administrativas",
    APLICACAO : "Aplicacao",

    OUTROS : "Outros"
}

class Calendario {
    constructor(ano, mes, diaDoMes){
        this.ano = ano
        this.mes = mes
        this.diaDoMes = diaDoMes
    }
}

class Transacao{
    constructor(parserObject){
            this.data = this.buildData(parserObject.datas)
            this.textoIdentificador = parserObject.textoIdentificador
            this.valor = parserObject.valor
            this.descricao = parserObject.descricao
            this.numeroDoc = parserObject.numeroDOC
            this.tipos = parserObject.tipos
        }

    buildData(dt) {
        return new Calendario(dt.year, dt.month, dt.dayOfMonth)
    }
}
