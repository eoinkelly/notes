class AppDrawer extends HTMLElement {
  constructor() {
    super(); // required
    window.console.log('constructor');

    // create property on this class and set default. This does not magically
    // read the "name" attribute from the element.
    this.name = 'World';
    this.shadow = null;
  }

  // properties of this class that the browser should observe
  static get observedAttributes() {
    return ['name'];
  }

  // this is fired when some main page JS changes the property on the element
  // ?? is it also changed when the attribute is changed internally?
  attributeChangedCallback(property, oldValue, newValue) {
    console.log('attributeChangedCallback', arguments);
    if (oldValue === newValue) return;
    this[property] = newValue;

    if (this.shadow) {
      // ?? cleaner way to do this?
      this.updateContent(newValue);
    }
  }

  // called when the element is adopted into a new document - this is an edge case
  adoptedCallback() {
    console.log('adoptedCallback arguments :>> ', arguments);
  }

  // called when the element is disconnected e.g. removed from the DOM
  disconnectedCallback() {
    console.log('disconnectedCallback arguments :>> ', arguments);
  }

  // connect component
  connectedCallback() {
    console.log('connecctedCallback');
    console.log('this.name :>> ', this.name);

    this.shadow = this.attachShadow({ mode: 'closed' });

    this.shadow.innerHTML = `
				<style>
					/* CSS from outside can still override :host because the element is in the DOM of the main page too */
					:host {
						outline: 3px solid green !important;
						display: block; /* needed - by default it's inline */
					}

					/* this does not work - use :host */
					/* app-drawer { outline: 5px solid yellow; } */
				</style>

				<p class="data">Hello World!</p>
			`;

    this.updateContent(this.name);
  }

  updateContent(newVal) {
    this.shadow.querySelector('.data').textContent = `Hello ${newVal}!`;
  }
}

window.customElements.define('app-drawer', AppDrawer);
