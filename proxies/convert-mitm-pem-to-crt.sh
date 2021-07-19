#!/usr/bin/env bash

set -x
set -e

MITM_PEM="${HOME}/.mitmproxy/mitmproxy-ca-cert.pem"
MITM_CRT="./mitm.crt"

openssl x509 -in "${MITM_PEM}" -inform PEM # -out ${MITM_CRT}
