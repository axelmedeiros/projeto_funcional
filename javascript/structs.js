class Calendario {
    constructor(ano, mes, diaDoMes){
        this.ano = ano
        this.mes = mes
        this.diaDoMes = diaDoMes
    }
}

class Transacao{
    constructor(parserObject){
            this.data = this.buildData(parserObject.data)
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
