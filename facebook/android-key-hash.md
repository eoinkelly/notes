# Facebook developer hashes

> If you run the sample apps that use Facebook Login, you need to add your
> Android development key hash to your Facebook developer profile.

> This is because Facebook uses the key hash to _authenticate_ interactions
> between your app and the Facebook app.

QUESTION: so it is interactions between both apps on the same device ???

# Storage of key-hash

The key-hash is stored in the app's "android settings" on facebook.com

There is also a key-hash in your facebook developer profile but I think this is
just for the sample facebook android app.

# To generate a key-hash

Use

```
keytool -exportcert -alias androiddebugkey -keystore ~/.android/debug.keystore | openssl sha1 -binary | openssl base64
# you are prompted for the _keystore_ password but it does not seem to be
# required (you get same output if you omit it)
```

WARNING: you must specify the correct alias for the key in a keystore when
generating the key or the output will be incorrect. Unfortunately you will
receive no indication that something is wrong!
