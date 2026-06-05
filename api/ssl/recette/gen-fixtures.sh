#!/bin/sh
# Generate test fixtures for SSL recette
cd "$(dirname "$0")"

if ! command -v openssl >/dev/null 2>&1; then
  echo "ERROR: openssl not found. Install OpenSSL to generate test fixtures." >&2
  exit 1
fi

openssl req -x509 -newkey rsa:2048 -keyout recette-key.pem -out recette-cert.pem \
  -days 1 -nodes -subj '/CN=test' 2>/dev/null

openssl rsa -in recette-key.pem -out recette-key-pkcs1.pem -traditional 2>/dev/null

openssl pkcs12 -export -in recette-cert.pem -inkey recette-key.pem \
  -out recette.p12 -passout pass:testpass 2>/dev/null

echo "01" > recette-crlnum
cat > recette-ca.cnf <<EOF
[ca]
default_ca=CA_default
[CA_default]
database=/dev/null
crlnumber=recette-crlnum
default_crl_days=1
default_md=sha256
EOF
openssl ca -gencrl -keyfile recette-key.pem -cert recette-cert.pem \
  -out recette.crl -config recette-ca.cnf 2>/dev/null
rm -f recette-ca.cnf recette-crlnum recette-crlnum.old

echo "Test fixtures generated."
