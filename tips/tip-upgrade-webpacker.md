
bundle update webpacker
rails webpacker:binstubs
yarn upgrade @rails/webpacker --latest
yarn upgrade webpack-dev-server --latest

Check that .babelrc isn't broken - I had problems with this
