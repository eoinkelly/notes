# ~/.android/debug.keystore

* a "Java KeyStore" file
* installed by the android SDK tools
* the keystore itself has a password and any private keys in it also have passwords
* this file is the debug keystore so it has a known password so you don't have
  to mess with signing while in dev mode
* it seems you can do "read" operations to a keystore without providing its password
* default settings for the debug keystore
    ```
    Keystore name: "debug.keystore"
    Keystore password: "android"
    Key alias: "androiddebugkey"
    Key password: "android"
    CN: "CN=Android Debug,O=Android,C=US"
    ```
* Accordign to SO this will manually rebuild a keystore
    ```
    keytool -genkey -v -keystore ~/.android/debug.keystore -storepass android -alias androiddebugkey -keypass android -dname "CN=Android Debug,O=Android,C=US"
    ```

To inspect a keystore:

```
# it seems you can list the contents of a keystore if you don't give a password
# (you just get a warning)

$ keytool  -keystore ~/.android/debug.keystore -list
Enter keystore password:

Keystore type: JKS
Keystore provider: SUN

Your keystore contains 1 entry

androiddebugkey, 3/07/2014, PrivateKeyEntry,
Certificate fingerprint (SHA1): 2A:77:34:5D:E3:17:C2:EB:EA:11:6F:4B:E9:E7:4D:EF:F9:1A:7C:5D
```

