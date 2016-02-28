#!/bin/bash

# A helper script to sign an android APK file for the play store (because I can
# never remember the command args)

set -eu

KEYSTORE_PATH="$HOME/path/to/android-keystore.jks"
UNSIGNED_APK_FILE="./Someapp-release-unsigned.apk"
EMAIL="foo@bar.com"

# You must have jarsigner available
if ! which -s jarsigner; then
  echo "It seems that jarsigner is not installed or not available on your path"
  exit 1
fi

# You must have zipalign available
if ! which -s zipalign; then
  echo "It seems that zipalign is not installed or not available on your path"
  exit 1
fi

# You must have access to the keystore
if [ ! -f "${KEYSTORE_PATH}" ]; then
  echo "You must have the rabid android keystore file at the expected location at ${KEYSTORE_PATH}"
  exit 1
fi

jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore "$KEYSTORE_PATH" "$UNSIGNED_APK_FILE" "$EMAIL"
zipalign -v 4 $UNSIGNED_APK_FILE ./Yourapp-`date +%Y-%m-%d_%H-%M-%S`.apk
