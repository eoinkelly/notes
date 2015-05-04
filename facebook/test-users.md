# Programmatically deauthorize an app for a test user

1. get an app access token
    ```sh
    GET /oauth/access_token?
        client_id={app-id}
        &client_secret={app-secret}
        &grant_type=client_credentials
    # the returned JSON has an access token for the app
    ```
2. get the list of test users
    ```sh
    https://graph.facebook.com/v2.3/{app-id}/accounts/test-users?access_token={app-access-token-from-previous-step}
    ```
    Each user in this list will have `access_token` fields for those users who
    have authorized your app

3. delete the permissions
    ```sh
    # deauthorize the app (the access token encodes which app we are getting permissions for)
    DELETE https://graph.facebook.com/v2.3/{user-id}/permissions?access_token={user-access-token}

    ```
    Now if you repeat step 2 you should see that this user no longer has an `access_token` attribute


# Programmatically authorize an app for a test user

We can do it via the FB developers console via the poorly named "get access token for this test user"

We have to use a facebook dialog to login as the user and then request that the give permissions to the app

