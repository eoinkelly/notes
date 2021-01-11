#!/usr/bin/env ruby

fail <<~EO_ERROR
  If you are wanting to renew these certs then this script won't help. This
  script creates new private key + CSR pairs for each desired cert - this is
  useful when initially registering a cert but for renewal you will want to
  re-use your existing private keys.

  Remove this line if you really need to run this script!
EO_ERROR

# Helper script to generate private keys and CSRs for the required certs
CERTS = [
  {
    file_stem: "Production",
    common_name: "*.example.com"
  },
]

COMMON = {
  country: "NZ",
  state: "Wellington",
  location: "Wellington",
  org: "Client Name",
  org_unit: "Org Name"
}

OUTPUT_PATH = __dir__

CERTS.each do |cert_info|
  file_stem = cert_info[:file_stem]
  common_name = cert_info[:common_name]
  subject_line = "/C=#{COMMON[:country]}/ST=#{COMMON[:state]}/L=#{COMMON[:location]}/O=#{COMMON[:org]}/OU=#{COMMON[:org_unit]}/CN=#{common_name}"

  Dir.chdir(OUTPUT_PATH) do
    # inspired by https://www.digicert.com/easy-csr/openssl.htm
    system "openssl req -new -newkey rsa:2048 -nodes -out '#{file_stem}.csr' -keyout '#{file_stem}.key' -subj '#{subject_line}'"
  end
end
