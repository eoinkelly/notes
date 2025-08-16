```js
this['App']['Templates']['disclaimer'] = Handlebars.template({
    compiler: [6, '>= 2.0.0-beta.1'],
    main: function (depth0, helpers, partials, data) {
        return '<h2>Disclaimer</h2>\n\nDisclaimer content goes here.\n';
    },
    useData: true
});

this['App']['Templates']['header'] = Handlebars.template({
    1: function (depth0, helpers, partials, data) {
        return '    <img id="show-menu" class="show-menu" src="images/hamburgermenu.png">\n';
    },
    3: function (depth0, helpers, partials, data) {
        var helper,
            functionType = 'function',
            helperMissing = helpers.helperMissing,
            escapeExpression = this.escapeExpression;
        return (
            '    <button id="back-button" class="back-button">' +
            escapeExpression(
                ((helper =
                    (helper =
                        helpers.backButtonText ||
                        (depth0 != null ? depth0.backButtonText : depth0)) !=
                    null
                        ? helper
                        : helperMissing),
                typeof helper === functionType
                    ? helper.call(depth0, {
                          name: 'backButtonText',
                          hash: {},
                          data: data
                      })
                    : helper)
            ) +
            '</button>\n'
        );
    },
    5: function (depth0, helpers, partials, data) {
        return '    <button id="cancel-button" class="cancel-button">Cancel</button>\n';
    },

    7: function (depth0, helpers, partials, data) {
        return '    <img id="close-button" class="close-button" src="images/iconCrossOrange@2x.png" />\n';
    },

    9: function (depth0, helpers, partials, data) {
        var helper,
            functionType = 'function',
            helperMissing = helpers.helperMissing,
            escapeExpression = this.escapeExpression;
        return (
            '    <div class="title title-large title-' +
            escapeExpression(
                ((helper =
                    (helper =
                        helpers.titleTag ||
                        (depth0 != null ? depth0.titleTag : depth0)) != null
                        ? helper
                        : helperMissing),
                typeof helper === functionType
                    ? helper.call(depth0, {
                          name: 'titleTag',
                          hash: {},
                          data: data
                      })
                    : helper)
            ) +
            '"></div>\n'
        );
    },

    11: function (depth0, helpers, partials, data) {
        var helper,
            functionType = 'function',
            helperMissing = helpers.helperMissing,
            escapeExpression = this.escapeExpression;
        return (
            '    <div class="title title-' +
            escapeExpression(
                ((helper =
                    (helper =
                        helpers.titleTag ||
                        (depth0 != null ? depth0.titleTag : depth0)) != null
                        ? helper
                        : helperMissing),
                typeof helper === functionType
                    ? helper.call(depth0, {
                          name: 'titleTag',
                          hash: {},
                          data: data
                      })
                    : helper)
            ) +
            '"></div>\n'
        );
    },

    13: function (depth0, helpers, partials, data) {
        return '    <button class="done-button">Done</button>\n';
    },

    15: function (depth0, helpers, partials, data) {
        return '  <hr class="divider"/>\n';
    },

    compiler: [6, '>= 2.0.0-beta.1'],

    main: function (depth0, helpers, partials, data) {
        var stack1,
            buffer = '<div class="lhs">\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showMenuButton : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(1, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer += '\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showBackButton : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(3, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer += '\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showCancelButton : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(5, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer += '\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showCloseButton : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(7, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer +=
            '</div>\n\n<!-- Possible titleTag values: -->\n<!-- appfeedback -->\n<!-- disclaimer -->\n<!-- enteramount -->\n<!-- fish4all -->\n<!-- fishspecies -->\n<!-- helpfullinks -->\n<!-- hookyourmates -->\n<!-- links -->\n<!-- logcatch -->\n<!-- myfishingdiary -->\n<!-- myphotos -->\n<!-- potapoacher -->\n<!-- settings -->\n<!-- shareaphoto -->\n<!-- statistics -->\n\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showLargeTitle : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(9, data),
                inverse: this.program(11, data),
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer += '\n<div class="rhs">\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showDoneButton : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(13, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        buffer += '</div>\n\n';
        stack1 = helpers['if'].call(
            depth0,
            depth0 != null ? depth0.showDivider : depth0,
            {
                name: 'if',
                hash: {},
                fn: this.program(15, data),
                inverse: this.noop,
                data: data
            }
        );
        if (stack1 != null) {
            buffer += stack1;
        }
        return buffer;
    },

    useData: true
});
```

Compiled templates file contains a bunch of invocations of
`Handlebars.template()` that are assigned to attributes of a global.

### `Handlebars.template()`

- Takes a single object as a param
- the param object contains attributes
    - compiler (Array)
    - main (Function) returns the template string
    - useData (Bool)
    - 0 or more functions on odd numbered keys (Function)
- returns a function which is the compiled template

The main function and the odd-numered-key funcitons all have the same signature

- take 4 params:
    - depth0
    - helpers
    - partials
    - data
- `this` is ???
- return a string

The compiled functions returned by `Handlebars.Template()` all look like the
following:

```
// this is what all compiled template function looks like
 function (context, options) {
      options = options || {};
      var data = options.data;

      ret._setup(options);
      if (!options.partial && templateSpec.useData) {
        data = initData(context, data);
      }
      var depths;
      if (templateSpec.useDepths) {
        depths = options.depths ? [context].concat(options.depths) : [context];
      }

      return templateSpec.main.call(container, context, container.helpers, container.partials, data, depths);
    }
```

I can only assume that the Handlebars.template returns this function but it
still has access to the variables from the closure so each one has access to
different variables such as:

- templateSpec
- ret
- container
