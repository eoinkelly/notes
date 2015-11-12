# Linux user admin

Sources

* https://help.ubuntu.com/community/AddUsersHowto#Command-line

```sh
# see which groups a user is in
groups <username>

# create new user
sudo adduser <username> --ingroup admin

# delete existing user
sudo deluser --remove-home <username>
```

## Higher level tools

* adduser
* addgroup

Reasons to use the higher level tools

* they automatically follow Debian policy for UID and GID values
* they create a home dir for the new user with skeletal configuration

## Low level tools

* useradd
* usermod
* groupadd

The lower level tools cover some edge-cases that aren't possible to cover with
the higher level tools.
