-module(pkcs10).
-export([create_csr/3]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("c_lib/pkcs10_drv", 0).

create_csr(_, _, _) ->
  exit(nif_library_not_loaded).
