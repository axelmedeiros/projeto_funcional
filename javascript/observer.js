let data = null 

fetch("http://150.165.15.10:8080/todasTransacoes",{method: 'POST'})
    .then((response) => response.json())
    .then((json) => data = json.map(obj => new Transacao(obj)))
    

const functionSelector = document.querySelector("#op")
const parametros = document.querySelector("#parametros")
const action = document.querySelector("#go")


const generateHtml = (genericResponse) => {
    let response = null

    if(typeof genericResponse == "number") {
        response = [document.createElement("p")]
        response[0].textContent = genericResponse
    } else if(genericResponse[0] instanceof Transacao) {
        response = genericResponse.map(t => new TransactionWC(t.data.ano,t.data.mes, t.data.diaDoMes, t.textoIdentificador, t.valor, t.numeroDoc, t.tipos, t.descricao))
    } else {
        response = [document.createElement("ul")]
        const lis = genericResponse.map(obj => {
            const day = obj["day"]
            const dayBalance = obj["dayBalance"]

            return `<li>day: ${day}, balance: ${dayBalance}</li>`
        })
        response[0].innerHTML  = lis.reduce((t, l) => t.concat(l), "")
    }

    return response
}

const renderResponse = (local, content) => {
    local.innerHTML = ""
    content.forEach(element => {
        local.append(element)
    })
    
}


action.onclick = () => {
    alert("carregandos os resultados")
    setInterval(() => {
        const calledFunction = functionSelector.value
        const parms = parametros.value.split(",")
        const funcao = functionsCarrier[calledFunction]
        let response = null
        if(parms.length === 1)
            response = funcao(data, parseInt(parms[0]))
        else if(parms.length === 2)
            response = funcao(data, parseInt(parms[0]), parseInt(parms[1]))
        else
            alert("error")

        const local = document.getElementById("response")
        renderResponse(local,generateHtml(response))
    }, 3000)
    

}
