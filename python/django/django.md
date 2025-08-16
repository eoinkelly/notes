# Django

- Django project contains many Django apps
    - each app is a python package (a directory with an `__init__.py`)
    - apps can live anywhere on your `PYTHONPATH`
- has a local dev server
- prod servers can be `ASGI` or `WSGI` convention
    - What servers are used in prod?
- Includes authentication and authorization (built-in devise and pundit)
- Current latest is 5.0.2
- organised by sub-apps
    - `runserver` seems to look for at all the python packages (folders with
      `__init__.py` ) in cwd and run them
    - there is a "core" django app (aka python package) created with same name
      as project
        - it does not have migrations sub dir unlike other sub apps - why?
            - those migration files seem to be built-in to the django package.
              The `django_migrations` table does track their application
        - the name of the project and core package (which are same) is
          hard-coded into files in the core package
        - the app has the followign modules
            1. settings
            1. urls
            1. wsgi
            1. asgi
- urls are routed to "Views" via the `urls` python module in each app
    - django views seem to act like rails controllers a bit
- the inital django migration creates tables for
    - auth (users, groups, permissions)
    - sessions
        - TODO: sessions are tracked in DB by default?
    - applied migrations
    - MIME content types
    - admin log
        - stores an audit log of actions taken by admins
- no user is automatically created
- Built-in authentication and authorisation
    - `/admin` is the URL for user sign-in
        - gives you a basic "rails admin" style admin
        - you can manage users and groups out of the box
    - how to set default password constraints for auth?
        - they don't seem super strong by default???
- https://www.pyinvoke.org/ seems to be the Python rake
    - not included in the default django install but is in our work one
- django doesn't create any package management files by default
    - you install the packages however you want and then edit `INSTALLED_APPS`
      in settings to tell Django about it.
- https://django-extensions.readthedocs.io/en/latest/
    - fills in some tools I miss from Rails e.g. `show_urls`
- views
    - most of the logic in an app is defined in views
    - view can be a function or a class
    - gets a `HttpRequest` instance and returns a `HttpResponse` instance
- templates
    - django has it's own Django Template Language (DTL) but also ships with
      Jinja2 support
    - we seem to use [jinja2](https://jinja.palletsprojects.com/en/3.1.x/) with
      `.html` file extensions
- jinja2 https://jinja.palletsprojects.com/en/3.1.x/
    - has for loops, if/else, macros, can set variables
- migrations
    - are built automatically from your models via `makemigrations` command.
      `migrate` command applies the migrations
- apps
    - all the models, views, templates required by the app
    - an app is a single design domain in DDD
    - book says you should be able to move the app to another project if well
      designed - is that realistic?

## manage.py

- gives access to various commands
- contains all the commands that `django-admin` provides and more

```bash
$ python manage.py --help
#
# Type 'manage.py help <subcommand>' for help on a specific subcommand.
#
# Available subcommands:
#
# [auth]
#     changepassword
#     createsuperuser
#
# [contenttypes]
#     remove_stale_contenttypes
#
# [django]
#     check
#     compilemessages
#     createcachetable
#     dbshell
#     diffsettings
#     dumpdata
#     flush
#     inspectdb
#     loaddata
#     makemessages
#     makemigrations
#     migrate
#     optimizemigration
#     sendtestemail
#     shell
#     showmigrations
#     sqlflush
#     sqlmigrate
#     sqlsequencereset
#     squashmigrations
#     startapp
#     startproject
#     test
#     testserver
#
# [sessions]
#     clearsessions
#
# [staticfiles]
#     collectstatic
#     findstatic
#     runserver

$ django-admin --help
# Type 'django-admin help <subcommand>' for help on a specific subcommand.
#
# Available subcommands:
#
# [django]
#     check
#     compilemessages
#     createcachetable
#     dbshell
#     diffsettings
#     dumpdata
#     flush
#     inspectdb
#     loaddata
#     makemessages
#     makemigrations
#     migrate
#     optimizemigration
#     runserver
#     sendtestemail
#     shell
#     showmigrations
#     sqlflush
#     sqlmigrate
#     sqlsequencereset
#     squashmigrations
#     startapp
#     startproject
#     test
#     testserver
# Note that only Django core commands are listed as settings are not properly configured (error: Requested setting INSTALLED_APPS, but settings are not configured. You must either define the environment variable DJANGO_SETTINGS_MODULE or call settings.configure() before accessing settings.).
#

$ python manage.py shell # rails console equivalent
$ python manage.py test # run tests
```

- Has a `squashmigrations` command which seems cool addition

## default routes from fresh django app

```bash
❯ python manage.py show_urls
/admin/ django.contrib.admin.sites.index        admin:index
/admin/<app_label>/     django.contrib.admin.sites.app_index    admin:app_list
/admin/<url>    django.contrib.admin.sites.catch_all_view
/admin/auth/group/      django.contrib.admin.options.changelist_view    admin:auth_group_changelist
/admin/auth/group/<path:object_id>/     django.views.generic.base.RedirectView
/admin/auth/group/<path:object_id>/change/      django.contrib.admin.options.change_view        admin:auth_group_change
/admin/auth/group/<path:object_id>/delete/      django.contrib.admin.options.delete_view        admin:auth_group_delete
/admin/auth/group/<path:object_id>/history/     django.contrib.admin.options.history_view       admin:auth_group_history
/admin/auth/group/add/  django.contrib.admin.options.add_view   admin:auth_group_add
/admin/auth/user/       django.contrib.admin.options.changelist_view    admin:auth_user_changelist
/admin/auth/user/<id>/password/ django.contrib.auth.admin.user_change_password  admin:auth_user_password_change
/admin/auth/user/<path:object_id>/      django.views.generic.base.RedirectView
/admin/auth/user/<path:object_id>/change/       django.contrib.admin.options.change_view        admin:auth_user_change
/admin/auth/user/<path:object_id>/delete/       django.contrib.admin.options.delete_view        admin:auth_user_delete
/admin/auth/user/<path:object_id>/history/      django.contrib.admin.options.history_view       admin:auth_user_history
/admin/auth/user/add/   django.contrib.auth.admin.add_view      admin:auth_user_add
/admin/autocomplete/    django.contrib.admin.sites.autocomplete_view    admin:autocomplete
/admin/jsi18n/  django.contrib.admin.sites.i18n_javascript      admin:jsi18n
/admin/login/   django.contrib.admin.sites.login        admin:login
/admin/logout/  django.contrib.admin.sites.logout       admin:logout
/admin/password_change/ django.contrib.admin.sites.password_change      admin:password_change
/admin/password_change/done/    django.contrib.admin.sites.password_change_done admin:password_change_done
/admin/r/<int:content_type_id>/<path:object_id>/        django.contrib.contenttypes.views.shortcut      admin:view_on_site
```

## Running django in production

- Usually deployed behind a reverse proxy like nginx
- [gunicorn](https://gunicorn.org/) seems popular
    - a python re-implementation of Unicorn in Ruby

## async django

- https://docs.djangoproject.com/en/5.0/topics/async/
    - seems like there are some constraints
        - all middleware you use must support async too
        - there doesn't seem to be full support for the ORM
        - it seems there is some disagreement in the community about how async
          django can become
        - https://github.com/encode/django-rest-framework/discussions/7774#discussioncomment-6361530
          seems like a good summary from July 2023
- Async seems to be done by using https://www.uvicorn.org/ as a worker type
  within https://gunicorn.org/
- We seem to mostly just use async unicorn at work but there is support for
  async in the template and poetry file
- Async seems to be required for websockets

## websockets

https://channels.readthedocs.io/en/latest/
