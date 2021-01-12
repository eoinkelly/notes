#!/usr/bin/env ruby

OPENSSL_REQ_CONFIG_FILEPATH = File.absolute_path(File.join(__dir__, "tmp/req.conf"))
OUTPUT_CSR_PATH = File.absolute_path(File.join(__dir__, "tmp/example.csr"))
OUTPUT_CSR_DESCRIPTION_PATH = File.absolute_path(File.join(__dir__, "tmp/example.csr.description.txt"))
OUTPUT_PRIVATE_KEY_PATH = File.absolute_path(File.join(__dir__, "tmp/example.key"))

OPENSSL_REQ_CONFIG = <<~EOM
  [req]
  default_bits = 2048
  distinguished_name = req_distinguished_name
  req_extensions = v3_req
  prompt = no

  [req_distinguished_name]
  C = US
  ST = VA
  L = City
  O = YourOrganization
  OU = YourOrganizationUnit
  CN = www.example.com

  [v3_req]
  keyUsage = keyEncipherment, dataEncipherment
  extendedKeyUsage = serverAuth
  subjectAltName = @alt_names

  # Add more lines to this section if you have more domains to add
  [alt_names]
  # one of your alt_names should duplicate the domain in CN above
  DNS.1 = www.example.com
  DNS.2 = example.com
  DNS.3 = sub.example.com
  DNS.4 = sub2.example.net
EOM


def log(msg)
  STDERR.puts(msg)
end

log "Writing OpenSSL config file to #{OPENSSL_REQ_CONFIG_FILEPATH}"
File.write(OPENSSL_REQ_CONFIG_FILEPATH, OPENSSL_REQ_CONFIG)

# create the CSR
log "Writing CSR to #{OUTPUT_CSR_PATH}"
log "Writing Private Key to #{OUTPUT_PRIVATE_KEY_PATH}"
system "openssl req -new -out #{OUTPUT_CSR_PATH} -newkey rsa:2048 -nodes -sha256 -keyout #{OUTPUT_PRIVATE_KEY_PATH} -config #{OPENSSL_REQ_CONFIG_FILEPATH}"

description = `openssl req -noout -text -in #{OUTPUT_CSR_PATH}`

# verify the CSR and dump info to stdout
log "Dumping info about CSR to #{OUTPUT_CSR_DESCRIPTION_PATH}:"
File.write(OUTPUT_CSR_DESCRIPTION_PATH, description)

log "Dumping info about CSR to screen:"
puts description
