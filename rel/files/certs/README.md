# Certificates directory

This directory should contain WEB key/cert pair and (optionally) a
chain of intermediate CA's for the certificate, under the following names:

* web_cert.pem
* web_key.pem
* web_chain.pem

The default paths can be changed in *app.config* file.

### Default test certificates

By default, this directory contains test certificates that are signed by
Onedata Test CA and issued for the "localhost" domain. They serve only for 
testing purposes and must be replaced in production.

### Certs sharing with Oneprovider/Onezone

The certificates placed here will be used both by onepanel and by underlying
Oneprovider / Onezone.

#### NOTE

You should use certificates signed by a trusted CA to ensure secure connections 
for clients.
