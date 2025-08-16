# JS Framework size comparison

## Individual pieces

- react (no addons)
    - min: 122 kB
    - min + gzip: 35 KB
- angular
    - minified: 141 KB
    - min + gzip: 50 KB
- ember
    - minified: 402 kB
    - min + gzip: 105 kB
- jquery 2.1.4
    - minified: 82 KB
    - min + gzip: 29 KB
- jquery 1.11.3
    - minified: 94 KB
    - min + gzip: 33 KB
- Underscore
    - minified: 16 KB
    - min + gzip: 5.7 KB
- Backbone
    - minified: 22 KB
    - min + gzip: 7.3 KB
- Marionette
    - minified: 42 KB
    - min + gzip: 11 KB

# Total weights of frameworks + dependencies

- Ember + jQuery2:
    - min: 484 KB
    - min+gzip: 134 KB
- Angular + jQuery2:
    - min: 223 KB
    - min+gzip: 79 KB
- React + fluxxor + react-router
    - min: 122 kB + 31 KB + 34 KB = 177 KB
    - min + gzip: 35 KB + 9.3 KB + 8.9 KB = 53.2 KB
- Marionette:
    - min: 42 + 22 + 82 + 16 = 162 KB
    - min+gzip: 11 + 7.3 + 5.7 + 82 = 106 KB
