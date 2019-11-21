class TransactionWC extends HTMLElement{
    constructor(year, month, day,idtxt,value,ndoc, types, descricao){
        super();
        this.year = year
        this.month = month + 1
        this.day = day
        this.idtxt = idtxt
        this.value = value
        this.ndoc = ndoc
        this.descricao = descricao
        this.types = types
    }

    connectedCallback() {
        this.render()
    }


    render(){
        const html = this.getHtml()
        this.innerHTML = html
    }


    getHtml(){
        const myhtml = `
        <div class="transaction">
        <h>${this.idtxt}</h>
        <p>data: ${this.year}/${this.month}/${this.day}</p>
        <p>valor: ${this.value}</p>
        <p>numero do documento: ${this.ndoc}</p>
        <p>descrição: ${this.descricao}</p>
        <ul>${this.renderTypes()}</ul>
        </div>
        `
        return myhtml
    }

    renderTypes() {
        return this.types.map(t => `<li>${t}` + "</li>")
    }
}

window.customElements.define("transaction-wc", TransactionWC);
