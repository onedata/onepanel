# CA certs directory

This directory is used to add trusted certificates to the application.
By default, a standard CA bundle is used, so in most cases there
is no need to add additional certificates. If your environment uses its
own certification, your should add your trusted chain here.

To add certificates, just place files in this dir. Each file can contain any 
number of certificates. The application will read the whole dir, no matter the 
file names.

### Certs sharing with Oneprovider/Onezone

The certificates placed here will be used both by onepanel and by underlying
Oneprovider / Onezone.

#### NOTE

This directory is NOT used to add trusted chain to your web cert, in such
case please see the README in certs directory.
