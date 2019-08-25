

# setup the certificate authority
cd certificate-authority
cfssl gencert -initca ./ca-csr.json | cfssljson -bare ca
cd ..


