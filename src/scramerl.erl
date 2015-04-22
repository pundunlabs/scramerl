-module(scramerl).

-export([client_first_message/0]).


%% API

client_first_message(Username) ->
    gs2_header() ++ client_first_message_bare(Username).

client_first_message_bare(Username)->
    reserved_mext() ++ username(Username) ++ nonce() ++ extensions().

reserverd_mext() ->
    "".

username(Username) ->
    "n=" ++ Username.

nonce() ->
    "r=" ++ c_nonce() ++ s_nonce().

c_nonce() ->
    "".

s_nonce() ->
    "".

extensions() ->
    "".

gs2_header() ->
    gs2_cbind_flag("n") ++ "," ++ authzid() ++ ",".

gs2_cbind_flag({"p", CB_NAME}) ->
    lists:concat(["p=", CB_NAME]);
gs2_cbind_flag("n") ->
    "n";
gs2_cbind_flag("y") ->
    "y".

authzid() ->
    "".

     SaltedPassword  := Hi(Normalize(password), salt, i)
     ClientKey       := HMAC(SaltedPassword, "Client Key")
     StoredKey       := H(ClientKey)
     AuthMessage     := client-first-message-bare + "," +
                        server-first-message + "," +
                        client-final-message-without-proof
     ClientSignature := HMAC(StoredKey, AuthMessage)
     ClientProof     := ClientKey XOR ClientSignature
     ServerKey       := HMAC(SaltedPassword, "Server Key")
     ServerSignature := HMAC(ServerKey, AuthMessage)


%% Internals Functions
