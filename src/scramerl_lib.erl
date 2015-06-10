-module(scramerl_lib).

-export([parse/1,
	 hi/3,
	 hi2/3,
	 prune/2]).

%%--------------------------------------------------------------------
%% @doc
%% Hi function defined in RFC 5802. Arguments are the String, Salt and
%% Iteration count.
%% @end
%%--------------------------------------------------------------------
-spec hi(Str :: string() | binary(),
	 Salt :: string() | binary(),
	 I :: pos_integer()) ->
    binary().
hi(Str, Salt, I) when is_list(Str)->
    hi(list_to_binary(Str), Salt, I);
hi(Str, Salt, I) when is_list(Salt)->
    hi(Str, list_to_binary(Salt), I);
hi(Str, Salt, 1) ->
    crypto:hmac(sha, Str, <<Salt/binary,0,0,0,1>>);
hi(Str, Salt, I)  when is_integer(I), I > 1->
    U1 = crypto:hmac(sha, Str, <<Salt/binary,0,0,0,1>>),
    hi(Str, [U1], I, 1).

-spec hi(Str :: binary(),
	 Acc :: [binary()],
	 I :: pos_integer(),
	 N :: pos_integer()) ->
    binary().
hi(Str, [Uy], I, 1) ->
    Ux = crypto:hmac(sha, Str, Uy),
    hi(Str, [Ux, Uy], I, 2);
hi(_Str, [Uy, Uz], I ,I) ->
    crypto:exor(Uy, Uz);
hi(Str, [Uy, Uz], I, N) ->
    Ux = crypto:hmac(sha, Str, Uy),
    Exor = crypto:exor(Uy, Uz),
    hi(Str, [Ux, Exor], I, N+1).

%%--------------------------------------------------------------------
%% @doc
%% An alternative implementation of Hi function defined in RFC 5802.
%% Arguments are the String, Salt and Iteration count.
%% @end
%%--------------------------------------------------------------------
hi2(Str, Salt, I) when is_list(Str)->
    hi2(list_to_binary(Str), Salt, I);
hi2(Str, Salt, I) when is_list(Salt)->
    hi2(Str, list_to_binary(Salt), I);
hi2(Str, Salt, 1) ->
    crypto:hmac(sha, Str, <<Salt/binary,0,0,0,1>>);
hi2(Str, Salt, I) when is_integer(I), I > 1->
    hi_help2(Str,crypto:hmac(sha, Str, <<Salt/binary,0,0,0,1>>), I-1).
hi_help2(_Str, PrevUi, 0) ->
    PrevUi;
hi_help2(Str, PrevUi, I) ->
    crypto:exor(hi_help2(Str, crypto:hmac(sha, Str, PrevUi), I-1), PrevUi).

%%--------------------------------------------------------------------
%% @doc
%% Parse a SCRAM message.
%% @end
%%--------------------------------------------------------------------
-spec parse(Data :: binary() | string()) ->
    {ok, map()} | {error, Reason :: term()}.
parse(Data) when is_binary(Data) ->
    parse(binary_to_list(Data));
parse(Data) when is_list(Data) ->
    Tokens = string:tokens(Data, ","),
    case parse_tokens(Tokens, []) of
	{error, Reason} ->
	    {error, Reason};
	PropList ->
	    maps:from_list([{str, Data} | PropList])
    end;
parse(_)->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc
%% Parse tokens of a SCRAM message.
%% @end
%%--------------------------------------------------------------------
-spec parse_tokens(Tokens :: [string()], Acc :: [{atom(), term()}]) ->
    [{atom(), term()}].
parse_tokens([], Acc) ->
    Acc;
parse_tokens(["y" | Rest], []) ->
    {AUTHZID, Rest1} = check_authzid(Rest),
    Acc = [{message, "client-first-message"}, {'gs2-header', "y"},
	   {authzid, AUTHZID}],
    parse_tokens(Rest1, Acc);
parse_tokens(["n" | Rest], []) ->
    {AUTHZID, Rest1} = check_authzid(Rest),
    Acc = [{message, "client-first-message"}, {'gs2-header', "n"},
	   {authzid, AUTHZID}],
    parse_tokens(Rest1, Acc);
parse_tokens([[$p, $= | CB_NAME] | Rest], []) ->
    {AUTHZID, Rest1} = check_authzid(Rest),
    Acc = [{message, "client-first-message"},
	   {'gs2-header', "n"},
	   {'cb-name', CB_NAME},
	   {authzid, AUTHZID}],
    parse_tokens(Rest1, Acc);
parse_tokens([[$c,$= | ChannelBinding] | Rest], []) ->
    Acc = [{message, "client-final-message"},
	   {'channel-binding', ChannelBinding}],
    parse_tokens(Rest, Acc);
parse_tokens([[$m,$= | ReservedMext] | Rest], []) ->
    Acc = [{message, "server-first-message"},
	   {'reserved-mext', ReservedMext}],
    parse_tokens(Rest, Acc);
parse_tokens([[$r,$= | Nonce] | Rest], []) ->
    Acc = [{message, "server-first-message"},
	   {nonce, Nonce}],
    parse_tokens(Rest, Acc);
parse_tokens([[$e,$= | ServerErrorValue] | Rest], []) ->
    Acc = [{message, "server-last-message"},
	   {'server-error', ServerErrorValue}],
    parse_tokens(Rest, Acc);
parse_tokens([[$v,$= | Verifier] | Rest], []) ->
    Acc = [{message, "server-last-message"},
	   {verifier, Verifier}],
    parse_tokens(Rest, Acc);
parse_tokens([[$m,$= | ReservedMext] | Rest], Acc) ->
    NewAcc = [{'reserved-mext', ReservedMext} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([[$n,$= | RawSaslName] | Rest], Acc) ->
    SaslName = replace_2c_3d(RawSaslName, []),
    NewAcc = [{username, SaslName} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([[$r,$= | Nonce] | Rest], Acc) ->
    NewAcc = [{nonce, Nonce} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([[$s,$= | Salt] | Rest], Acc) ->
    NewAcc = [{salt, Salt} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([[$i,$= | IterationCount] | Rest], Acc) ->
    NewAcc = [{'iteration-count', list_to_integer(IterationCount)} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([[$p,$= | Proof] | Rest], Acc) ->
    NewAcc = [{proof, Proof} | Acc],
    parse_tokens(Rest, NewAcc);
parse_tokens([Token | _Rest], _Acc) ->
    {error, {unknown_token, Token}}.
    
%%--------------------------------------------------------------------
%% @doc
%% Parse if optional authzid attribute is present.
%% @end
%%--------------------------------------------------------------------
-spec check_authzid(Tokens :: [string()]) ->
    {AUTHZID:: string() | undefined, Rest :: [string()]}.
check_authzid([[$a, $= | AUTHZID ] | Rest])->
    {AUTHZID, Rest};
check_authzid(Rest) ->
    {undefined, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% Replace "=2C" with "," and "=3D" with "=". Reutn saslname error
%% if any "=" char is not preceded with either  "2C" or "3D".
%% @end
%%--------------------------------------------------------------------
-spec replace_2c_3d(RawUserName :: string(), Acc :: string()) -> string().
replace_2c_3d([], Acc) ->
    lists:reverse(Acc);
replace_2c_3d([$=, $2, $C | Rest], Acc) ->
    replace_2c_3d(Rest, [$,|Acc]);
replace_2c_3d([$=, $3, $D | Rest], Acc) ->
    replace_2c_3d(Rest, [$=|Acc]);
replace_2c_3d([$= | _Rest], _Acc) ->
    {error, saslname};
replace_2c_3d([Char | Rest], Acc) ->
    replace_2c_3d(Rest, [Char | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Prune the given SCRAM Message string to produce either
%% client-first-message-bare of client-final-message-withouth-proof
%% @end
%%--------------------------------------------------------------------
-spec prune(Tag :: 'gs2-header' | proof, Str :: string()) ->
    string().
prune('gs2-header', Str) ->
    drop_gs2_header($n, $=, Str);
prune(proof, Str) ->
    drop_proof($,, $p, $=, Str, []).

drop_gs2_header(_, _, []) ->
    [];
drop_gs2_header(N, E, [N, E | Str]) ->
   [N, E | Str];
drop_gs2_header(N, E, [_C1, C2 | Str]) ->
    drop_gs2_header(N, E, [C2 | Str]). 

drop_proof(_, _, _, [], Acc) ->
    lists:reverse(Acc);
drop_proof(C, N, E, [C, N, E | _Str], Acc) ->
   lists:reverse(Acc);
drop_proof(C, N, E, [C1, C2, C3 | Str], Acc) ->
    drop_proof(C, N, E, [C2, C3 | Str], [C1 | Acc]). 
