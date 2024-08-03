// import {LitElement, html} from './node_modules/';
import {LitElement, html, css } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js';


export class AppDrawerLit extends LitElement {
  static properties = {
    name: {},
  };
  // Define scoped styles right with your component, in plain CSS
  static styles = css`
    :host {
      color: blue;
			display: block;
    }
  `;

  constructor() {
    super();
    // Declare reactive properties
    this.name = 'World';
  }

  // Render the UI as a function of component state
  render() {
    return html`<p>Hello, ${this.name}!</p>`;
  }
}


window.customElements.define('app-drawer-lit', AppDrawerLit);