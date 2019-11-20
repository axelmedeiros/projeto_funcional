let data = null 

fetch("http://150.165.15.10:8080/todasTransacoes",{method: 'POST'})
    .then((response) => response.json())
    .then((json) => data = json.map(obj => new Transacao(obj)))
    

const functionSelector = document.querySelector("#op")
const parametros = document.querySelector("#parametros")
const action = document.querySelector("#go")


action.onclick = () => {

    const calledFunction = functionSelector.value
    const parms = parametros.value.split(",")
    const funcao = functionsCarrier[calledFunction]
    if(parms.length === 1)
        console.log("repostas", funcao(data, parseInt(parms[0])))
    else if(parms.length === 2)
        console.log("respostas", funcao(data, parseInt(parms[0]), parseInt(parms[1])))
    else
        alert("error")
    
    

}