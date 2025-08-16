Note: `django-admin` doesn't create a README

## command record for starting this app

```bash
# https://docs.djangoproject.com/en/4.2/intro/tutorial01/

pip install Django
pyenv rehash
django-admin startproject eoin_test_site

cd eoin_test_site

# run dev server
python manage.py runserver

# run migrations (the migrations are not visible in my code? part of django?)
python manage.py migrate

# create a new app called polls
python manage.py startapp polls
```
