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

### Let's Encrypt account
If your deployment uses certificates obtained from Let's Encrypt by onepanel,
a subdirectory `letsencrypt` will be created to store Let's Encrypt
account credentials.

#### NOTE

You should use certificates signed by a trusted CA to ensure secure connections 
for clients.
