# Authentication in rails

## Devise

Provides controller helpers

```rb
current_user
user_session
user_signed_in?
authenticate_user! # suitable for use in a `before_action` filter

```

where `user` is the singular of the user model that devise is added to. If the
model is named `admin` then replace `user` with `admin in all the above.
