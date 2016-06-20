-module(scramerl).

-export([gs2_header/0,
	 client_first_message/1,
	 client_first_message_bare/1,
	 server_first_message/3,
	 client_final_message_without_proof/1,
	 client_final_message/3,
	 server_final_message/1,
	 gen_nonce/0,
	 client_proof/2,
	 server_signature/2]).


%%%===================================================================
%%% API
%%%===================================================================
%%% Taken from http://tools.ietf.org/html/rfc5802
%%% SaltedPassword  := Hi(Normalize(password), salt, i)
%%% ClientKey       := HMAC(SaltedPassword, "Client Key")
%%% StoredKey       := H(ClientKey)
%%% AuthMessage     := client-first-message-bare + "," +
%%%                    server-first-message + "," +
%%%                    client-final-message-without-proof
%%% ClientSignature := HMAC(StoredKey, AuthMessage)
%%% ClientProof     := ClientKey XOR ClientSignature
%%% ServerKey       := HMAC(SaltedPassword, "Server Key")
%%% ServerSignature := HMAC(ServerKey, AuthMessage)

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gs2_header() ->
    gs2_cbind_flag("n") ++ "," ++ authzid() ++ ",".

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec client_first_message(Username :: string()) -> string().
client_first_message(Username) when is_list(Username) ->
    gs2_header() ++ client_first_message_bare(Username).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec server_first_message(Nonce :: string(),
			   Salt :: string(),
			   IterationCount :: pos_integer()) -> string().
server_first_message(Nonce, Salt, IterationCount) ->
    reserved_mext() ++ nonce(Nonce) ++ "," ++ salt(Salt) ++ "," ++
    iteration_count(IterationCount) ++ extensions().

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec client_final_message_without_proof(Nonce :: string()) -> string().
client_final_message_without_proof(Nonce) ->
    channel_binding() ++ "," ++ nonce(Nonce) ++ extensions().

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec client_final_message(Nonce :: string(),
			   SaltedPassword :: string(),
			   AuthMessage :: string()) -> string().
client_final_message(Nonce, SaltedPassword, AuthMessage) ->
    Proof = client_proof(SaltedPassword, AuthMessage),
    client_final_message_without_proof(Nonce) ++ "," ++ proof(Proof).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec server_final_message({error, Reason :: term()}) -> string()
			; (Verifier :: string()) -> string().
server_final_message({error, Reason}) ->
    "e=" ++ Reason ++ extensions();
server_final_message(Verifier) when is_list(Verifier) ->
    "v=" ++ Verifier ++ extensions().

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
client_first_message_bare(Username)->
    reserved_mext() ++ username(Username) ++ "," ++ nonce(gen_nonce()) ++ extensions().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reserved_mext() ->
    "".

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
username(Username) ->
    "n=" ++ stringprep:prepare(Username).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
nonce(Nonce) ->
    "r=" ++ Nonce.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gen_nonce() ->
    [crypto:rand_uniform(48,125) || _ <- lists:seq(1,15)].

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
salt(Salt) ->
    "s=" ++ base64:encode_to_string(Salt).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
iteration_count(IterationCount) ->
    lists:concat(["i=", IterationCount]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
channel_binding() ->
    "c=" ++ base64:encode_to_string(gs2_header()).

proof(Proof)->
    "p=" ++ Proof.
%%--------------------------------------------------------------------
%% @doc
%% Return ClientProof attribute
%% ClientProof     := ClientKey XOR ClientSignature
%% @end
%%--------------------------------------------------------------------
client_proof(SaltedPassword, AuthMessage) ->
    ClientKey  = crypto:hmac(sha, SaltedPassword, <<"Client Key">>),
    StoredKey = crypto:hash(sha, ClientKey),
    ClientSignature = crypto:hmac(sha, StoredKey, AuthMessage),
    ClientProofBin = crypto:exor(ClientKey, ClientSignature),
    ClientProof = binary_to_list(ClientProofBin),
    base64:encode_to_string(ClientProof).

%%--------------------------------------------------------------------
%% @doc
%% Return ServerSignature attribute
%% ServerKey       := HMAC(SaltedPassword, "Server Key")
%% ServerSignature := HMAC(ServerKey, AuthMessage)
%%
%% @end
%%--------------------------------------------------------------------
server_signature(SaltedPassword, AuthMessage) ->
    ServerKey  = crypto:hmac(sha, SaltedPassword, <<"Server Key">>),
    ServerSignatureBin = crypto:hmac(sha, ServerKey, AuthMessage),
    base64:encode_to_string(binary_to_list(ServerSignatureBin)).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
extensions() ->
    "".

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gs2_cbind_flag({"p", CB_NAME}) ->
    lists:concat(["p=", CB_NAME]);
gs2_cbind_flag("n") ->
    "n";
gs2_cbind_flag("y") ->
    "y".

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
authzid() ->
    "".
