# AWS Cognito

How authentication happens:

1. Client sends `InitiateAuth`
2. Cognito replies with challenge
3. Client sends `RespondToAuthChallenge`
4. Go back to step 2 if there are more challenges
5. Cognito sends tokens

* We can use AWS Lambdas to customise the flow
    * Cognito can run lambdas to receive the `RespondToAuthChallenge`

Terminology

* SRP
    * Is a protocol
    * Has an RFC https://tools.ietf.org/html/rfc2945
    * https://en.wikipedia.org/wiki/Secure_Remote_Password_protocol
    > one party (the "client" or "user") demonstrates to another party (the
    > "server") that they know the password, without sending the password
    > itself, nor any other information from which the password can be broken.
    > The password never leaves the client and is unknown to the server.

There are 5 authentication flows

1. Client-side Auth flow
    * Designed for iOS, Android, Javascript in browser apps
    * Flow
        1. App calls `InitiateAuth` with users' username and "SRP details"
            * The app generates the SRP details by using the Amazon Cognito SRP support in the Android, iOS, and JavaScript SDKs.
        1. Cognito sends back a challenge
        1. The app calls `RespondToAuthChallenge` with its response
        1. If another challenge is required, respond with a "session" and go back 2 steps (the session lets Cognito link the challenges together
        1. If response is ok then Cognito issues tokens.
1. Server-side Auth flow
    * Designed for server apps
    * Flow
        1. App calls `AdminInitiateAuth` with Rsers' username and User's "SRP details" and AWS admin credentials
            * The app generates the SRP details by using the Amazon Cognito SRP support in the Android, iOS, and JavaScript SDKs.
        1. Cognito sends back "Authentication parameters"
        1. The app calls `AdminRespondToAuthChallenge` with its response (must also send AWS admin credentials)
        1. If another challenge is required, respond with a "session" and go back 2 steps (the session lets Cognito link the challenges together
        1. If response is ok then Cognito issues tokens.
1. Custom Auth flow
    * use this flow if you need to customise with Lambdas

1. Admin auth flow
1. User migration auth flow
    * Allows you to migrate users from another system using a "User migration lambda"


> After successfully authenticating a user, Amazon Cognito issues JSON web tokens (JWT) that you can use to secure and authorize access to your own APIs, or exchange for AWS credentials.
