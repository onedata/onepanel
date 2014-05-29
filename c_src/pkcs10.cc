#include <botan/init.h>
#include <botan/auto_rng.h>
#include <botan/x509self.h>
#include <botan/rsa.h>
#include <botan/dsa.h>
using namespace Botan;

#include <iostream>
#include <fstream>
#include <memory>

int create_csr(char* password, char* common_name, char* country, char* organization, char* email)
{
    Botan::LibraryInitializer init;
    try
    {
        AutoSeeded_RNG rng;
        RSA_PrivateKey priv_key(rng, 4096);
        std::ofstream key_file("priv.pem");
        key_file << PKCS8::PEM_encode(priv_key, rng, std::string(password));

        X509_Cert_Options opts;
        opts.common_name = std::string(common_name);
        opts.country = std::string(country);
        opts.organization = std::string(organization);
        opts.email = std::string(email);

        PKCS10_Request req = X509::create_cert_req(opts, priv_key, "SHA-256", rng);
        std::ofstream csr_file("csr.pem");
        csr_file << req.PEM_encode();
    }
    catch(std::exception& e)
    {
        std::cout << e.what() << std::endl;
        return 1;
    }
    return 0;
}
