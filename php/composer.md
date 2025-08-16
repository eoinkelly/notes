# Composer

- npm of PHP
- can be installed local to a project or globally
- distrubuted as a `composer.phar` binary
- `php composer.phar` to invoke it
- `composer.phar` is often renamed to `composer` and made executable
- reads `composer.json` in project root
- has `composer.lock` for version locks

```bash
cd path/to/project

# invoke a manually downloaded composer
php composer.phar install

# invoke one that has been setup more nicely
composer install # use this if you already have a lock file
```
