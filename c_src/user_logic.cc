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

#include <string>

std::string hash_password(const std::string &password, int work_factor)
{
    Botan::AutoSeeded_RNG rng;
    return Botan::generate_bcrypt(password, rng, work_factor);
}

bool check_password(const std::string &password, const std::string &hash)
{
    if (hash.length() != 60)
        return false;

    return Botan::check_bcrypt(password, hash);
}