# Devise Token authenticatable

This token can be given via

1. a query string or
2. HTTP Basic Authentication.

Strategy for signing in a user, based on a authenticatable token. This works
for both params and http. For the former, all you need to do is to pass the
    params in the URL:

http://myapp.example.com/?user_token=SECRET

For headers, you can use basic authentication passing the token as username and
blank password. Since some clients may require a password, you can pass “X” as
password and it will simply be ignored.
