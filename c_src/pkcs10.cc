#include <botan/init.h>
#include <botan/auto_rng.h>
#include <botan/x509self.h>
#include <botan/rsa.h>
#include <botan/dsa.h>
using namespace Botan;

#include <iostream>
#include <fstream>
#include <memory>

int foo(int x) {
Botan::LibraryInitializer init;

   try
      {
      AutoSeeded_RNG rng;

      RSA_PrivateKey priv_key(rng, 1024);

      std::ofstream key_file("private.pem");
      key_file << PKCS8::PEM_encode(priv_key, rng, "PASS");

      X509_Cert_Options opts;

      opts.common_name = "CN";
      opts.country = "PL";
      opts.organization = "ACK";
      opts.email = "DASDAS";

      PKCS10_Request req = X509::create_cert_req(opts, priv_key,
                                                 "SHA-256", rng);

      std::ofstream req_file("req.pem");
      req_file << req.PEM_encode();
   }
   catch(std::exception& e)
      {
      std::cout << e.what() << std::endl;
      return 1;
      }
  return x+1;
}

int bar(int y) {
  return y*2;
}
