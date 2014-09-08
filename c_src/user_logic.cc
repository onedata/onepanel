/*********************************************************************
*  @author Krzysztof Trzepla
*  @copyright (C): 2014 ACK CYFRONET AGH
*  This software is released under the MIT license
*  cited in 'LICENSE.txt'.
*  @end
**********************************************************************
*  @doc This is an implementation of Erlang NIF library described
*  in user_logic_nif interface.
*  @end
*********************************************************************/

#include <botan/auto_rng.h>
#include <botan/bcrypt.h>

using namespace Botan;

char* hash_password(char* password, int work_factor)
{
    AutoSeeded_RNG rng;
    std::string hash = generate_bcrypt(password, rng, work_factor);
    return &hash[0];
}

int check_password(char* password, char* hash)
{
    if(strlen(hash) != 60) {
        return 1;
    }

    int res = !check_bcrypt(password, hash);

    return res;
}