-module(stringprep).

%% Exported functions
-export([prepare/1,
	 prepare/2,
	 prepare/3]).

-export([saslprep_test/1]).
%%%===================================================================
%%% API
%%%===================================================================

saslprep_test(clean) ->
    code:purge(stringprep_lib),
    code:delete(stringprep_lib),
    saslprep_test(ok);
saslprep_test(_)->
    verbose([$I,16#00AD,$X], saslprep),
    verbose("user", saslprep),
    verbose("USER", saslprep),
    verbose([16#00AA], saslprep),
    verbose([16#2168], saslprep),
    verbose([16#0007], saslprep),
    verbose([16#0627,16#0031], saslprep),
    verbose([16#0627,16#0031,16#0628], saslprep).

verbose(Str, Profile)->
    io:format("stringprep(~p,~p) ->",[Str,Profile]),
    Result = stringprep:prepare(Str, Profile),
    io:format(" ~p~n",[Result]).

%%--------------------------------------------------------------------
%% @doc
%% Implements RFC 3454 "Preparation of Internationalized Strings
%% ("stringprep")".
%% Preperation does Mapping, Prohibiting, Normalization and
%% check of Bidirectional Characters.
%% @end
%%--------------------------------------------------------------------
-spec prepare(String :: string()) ->
    {ok, String :: string()} | {error, Reason :: term()}.
prepare(String) ->
    Code = code:which(stringprep_lib),
    prepare(String, saslprep, Code).

-spec prepare(String :: string(), Profile :: undefined | saslprep) ->
    {ok, String :: string()} | {error, Reason :: term()}.
prepare(String, Profile) ->
    Code = code:which(stringprep_lib),
    prepare(String, Profile, Code).

-spec prepare(String :: string(),
	      Profile :: undefined | saslprep,
	      Code :: non_existing | term()) ->
    {ok, String :: string()} | {error, Reason :: term()}.
prepare(String, Profile, non_existing) ->
    create_code(),
    do_prepare(String, Profile);
prepare(String, Profile, _) ->
    do_prepare(String, Profile).

-spec do_prepare(String :: string(), Profile :: undefined | saslprep) ->
    {ok, String :: string()} | {error, Reason :: term()}.
do_prepare(String, Profile) ->
    Mapped = map(String, Profile),
    Normalized = normalize(Mapped, Profile),
    case prohibit(Normalized, Profile) of
	ok ->
	    case check_bidi(Normalized, Profile) of
		ok -> Normalized;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec map(String :: string(), Profile :: undefined | saslprep) ->
    string().
map(String, saslprep) ->
    lists:flatten(map_(saslmap, String));
map(String, _) ->
    lists:flatten(map_(map, String)).

-spec map_(Fun :: atom(), String :: string()) ->
    [integer() | [integer()]].
map_(Fun, [H | T]) ->
    case stringprep_lib:Fun(H) of
	map_to_nothing ->
	    map_(Fun, T);
	M ->
	    [M | map_(Fun, T)]
    end;
map_(_, []) ->
    [].

-spec normalize(String :: string(), Profile :: undefined | saslprep) ->
    string().
normalize(String, saslprep) ->
    unicode:characters_to_nfkc_list(String);
normalize(String, _) ->
    String.

-spec prohibit(String :: string(), Profile :: undefined | saslprep) ->
    ok | {error, Reason :: term()}.
prohibit([H|T], Profile) ->
    case stringprep_lib:is_prohibited(H) of
	false -> prohibit(T, Profile);
	true -> {error, prohibited}
    end;
prohibit([], _)->
    ok.

-spec check_bidi(String :: string(), Profile :: undefined | saslprep) ->
    ok | {error, Reason :: term()}.
check_bidi(String, _Profile) ->
    case check_bidi_case_1(String) of
	ok -> check_bidi_case_2_3(String);
	Error -> Error
    end.

-spec check_bidi_case_1(String :: string()) ->
    ok | {error, Reason :: term()}.
check_bidi_case_1([H|T]) ->
    case is_bidi_prohibited(H) of
	false -> check_bidi_case_1(T);
	true -> {error, bidi_case_1}
    end;
check_bidi_case_1([])->
    ok.

-spec check_bidi_case_2_3(String :: string()) ->
    ok | {error, Reason :: term()}.
check_bidi_case_2_3([H|T]) ->
    RandAlCat = stringprep_lib:is_randalcat(H),
    LCat = stringprep_lib:is_lcat(H),
    check_bidi_case_2_3(T, #{first => RandAlCat, randalcat => RandAlCat, lcat => LCat});
check_bidi_case_2_3([]) ->
    ok.

-spec check_bidi_case_2_3(String :: string(), State :: #{}) ->
    ok | {error, Reason :: term()}.
check_bidi_case_2_3([], _) ->
    ok;
check_bidi_case_2_3([L], #{first := false}) ->
    case stringprep_lib:is_randalcat(L) of
	true ->
	    %%Last char is RandAlCat char but first char is not RandAlCat.
	    {error, bidi_case_3};
	false ->
	    ok
    end;
check_bidi_case_2_3([L], #{first := true}) ->
    case stringprep_lib:is_randalcat(L) of
	true ->
	    ok;
	false ->
	    {error, bidi_case_3}
    end;
check_bidi_case_2_3([H|T], State = #{first := false}) ->
    case stringprep_lib:is_randalcat(H) of
	true ->
	    %%RandAlCat char present but first char is not RandAlCat.
	    {error, bidi_case_3};
	false ->
	    case stringprep_lib:is_lcat(H) of
		true ->
		    check_bidi_case_2_3(T, State#{lcat => true});
		false ->
		    check_bidi_case_2_3(T, State)
	    end
    end;
check_bidi_case_2_3([H|T], State = #{randalcat := false}) ->
    LCat = stringprep_lib:is_lcat(H),
    check_bidi_case_2_3(T, State#{lcat => LCat});
check_bidi_case_2_3([H|T], State = #{randalcat := true}) ->
    case stringprep_lib:is_lcat(H) of
	true ->
	    %%Both RandAlCat char and LCAR char is present.
	    {error, bidi_case_2};
	false ->
	    check_bidi_case_2_3(T, State)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates stringprep_lib module which has map and prohibit functions.
%% @end
%%--------------------------------------------------------------------
create_code()->
    PrivDir = code:priv_dir(scramerl),
    Config = #{mapping_file => filename:join(PrivDir,"stringprep_appb.cfg"),
	       prohibit_file => filename:join(PrivDir,"stringprep_appc.cfg"),
	       randalcat_file => filename:join(PrivDir,"stringprep_appd1.cfg"),
	       lcat_file => filename:join(PrivDir,"stringprep_appd2.cfg"),
	       saslprep_mapping_file => filename:join(PrivDir,"saslprep_mappings.cfg")},
    create_stringprep(Config).

%%--------------------------------------------------------------------
%% @doc
%% Creates stringprep_lib module which has map and prohibit functions.
%% @end
%%--------------------------------------------------------------------
create_stringprep(#{mapping_file := MapFile,
		    prohibit_file := ProhibitFile,
		    randalcat_file := RandAlCatFile,
		    lcat_file := LCatFile,
		    saslprep_mapping_file := SaslMapFile}) ->
    {ok, MapData} = file:read_file(MapFile),
    {ok, MapTokens, _} = erl_scan:string(binary_to_list(MapData)),
    {ok, Mappings} = stringprep_mp:parse(MapTokens),

    {ok, PData} = file:read_file(ProhibitFile),
    {ok, PTokens, _} = erl_scan:string(binary_to_list(PData)),
    {ok, PList} = stringprep_pp:parse(PTokens),

    {ok, RandAlCatData} = file:read_file(RandAlCatFile),
    {ok, RandAlCatTokens, _} = erl_scan:string(binary_to_list(RandAlCatData)),
    {ok, RandAlCatList} = stringprep_bidi:parse(RandAlCatTokens),

    {ok, LCatData} = file:read_file(LCatFile),
    {ok, LCatTokens, _} = erl_scan:string(binary_to_list(LCatData)),
    {ok, LCatList} = stringprep_bidi:parse(LCatTokens),

    {ok, SaslMapData} = file:read_file(SaslMapFile),
    {ok, SaslMapTokens, _} = erl_scan:string(binary_to_list(SaslMapData)),
    {ok, SaslMappings} = stringprep_mp:parse(SaslMapTokens),

    CEForms = make_stringprep_mod(#{mappings => Mappings,
				    prohibit_list => PList,
				    randalcat_list => RandAlCatList,
				    lcat_list => LCatList,
				    sasl_mappings => SaslMappings}),
    {ok, _, Beam } = compile:forms(CEForms, [from_core, binary]),
    code:load_binary(stringprep_lib, [], Beam).

%%--------------------------------------------------------------------
%% @doc
%% Make module 'stringprep_lib' with map/1 and prohibit/1 functions.
%% @end
%%--------------------------------------------------------------------
make_stringprep_mod(#{mappings := Mappings,
		      prohibit_list := PList,
		      randalcat_list := RandAlCatList,
		      lcat_list := LCatList,
		      sasl_mappings := SaslMappings}) ->
    ModuleName = cerl:c_atom(stringprep_lib),
    cerl:c_module(ModuleName,
		  [cerl:c_fname(map, 1),
		   cerl:c_fname(saslmap, 1),
		   cerl:c_fname(is_prohibited,1),
		   cerl:c_fname(is_randalcat,1),
		   cerl:c_fname(is_lcat,1),
		   cerl:c_fname(module_info, 0),
		   cerl:c_fname(module_info, 1)],
		  [make_map_fun(map, Mappings),
		   make_map_fun(saslmap, SaslMappings),
		   make_check_fun(is_prohibited, PList),
		   make_check_fun(is_randalcat, RandAlCatList),
		   make_check_fun(is_lcat, LCatList)] ++
		   mod_info(ModuleName)).

%%--------------------------------------------------------------------
%% @doc
%% Make mapping function.
%% @end
%%--------------------------------------------------------------------
make_map_fun(FName, Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_map_clauses(Arg1, Mappings),
    
    LastClause = cerl:c_clause([Else], cerl:c_atom(true), Arg1),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(FName, 1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for mapping function.
%% @end
%%--------------------------------------------------------------------
make_map_clauses(Arg1, Mappings) ->
    make_map_clauses(Arg1, Mappings,[]).
    
%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for mapping function.
%% @end
%%--------------------------------------------------------------------
make_map_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_map_clauses(Arg1, [[{integer, _Line, Val}, MapsTo ]|Rest], Acc) ->
    IntList = [cerl:c_int(Int) || {integer, _, Int} <- MapsTo],
    Clause = cerl:c_clause([cerl:c_int(Val)], cerl:c_atom(true), cerl:make_list(IntList)),
    make_map_clauses(Arg1, Rest, [Clause|Acc]);
make_map_clauses(Arg1, [[{integer, _Line, Val}]|Rest], Acc) ->
    Clause = cerl:c_clause([cerl:c_int(Val)], cerl:c_atom(true), cerl:c_atom(map_to_nothing)),
    make_map_clauses(Arg1, Rest, [Clause|Acc]);
make_map_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
    {error, io_lib:format("Syntax error: ~p", [Line])}.

%%--------------------------------------------------------------------
%% @doc
%% Make check function for prohibit and bidi checks.
%% @end
%%--------------------------------------------------------------------
make_check_fun(FName, Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_check_clauses(Arg1, Mappings),
    
    LastClause = cerl:c_clause([Else], cerl:c_atom(true), cerl:c_atom(false)),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(FName, 1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for prohibit and bidi check functions.
%% @end
%%--------------------------------------------------------------------
make_check_clauses(Arg1, Mappings) ->
    make_check_clauses(Arg1, Mappings,[]).
    
%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for check prohibit and bidi function.
%% @end
%%--------------------------------------------------------------------
make_check_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_check_clauses(Arg1, [[{integer, _Line, Start},{integer, _, End}]|Rest], Acc) ->
    True = cerl:c_atom(true),
    Val = cerl:c_var('Val'),
    Exp1 = make_call(erlang, '>=', [Val, cerl:c_int(Start)]),
    Exp2 = make_call(erlang, '=<', [Val, cerl:c_int(End)]),
    Exp =  make_call(erlang, 'and', [Exp1, Exp2]),
    Clause = cerl:c_clause([Val], Exp, True),
    make_check_clauses(Arg1, Rest, [Clause|Acc]);
make_check_clauses(Arg1, [[{integer, _Line, Val}]|Rest], Acc) ->
    True = cerl:c_atom(true),
    Clause = cerl:c_clause([cerl:c_int(Val)], True, True),
    make_check_clauses(Arg1, Rest, [Clause|Acc]);
make_check_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
    {error, io_lib:format("Syntax error: ~p", [Line])}.

%%--------------------------------------------------------------------
%% @doc
%% Make module_info/1 function.
%% @end
%%--------------------------------------------------------------------
mod_info(Name) ->
    M = cerl:c_atom(erlang),
    F = cerl:c_atom(get_module_info),
    Info0 = {cerl:c_fname(module_info, 0),
	     cerl:c_fun([], cerl:c_call(M, F, [Name]))},
    Key = cerl:c_var('Key'),
    Info1 = {cerl:c_fname(module_info, 1),
	     cerl:c_fun([Key], cerl:c_call(M, F, [Name, Key]))},
    [Info0, Info1].

%%--------------------------------------------------------------------
%% @doc
%% Make a function call cerl with given Mod, Fun, and Args.
%% @end
%%--------------------------------------------------------------------
make_call(Mod0, Fun0, Args) ->
    Mod = cerl:c_atom(Mod0),
    Fun = cerl:c_atom(Fun0),
    cerl:c_call(Mod, Fun, Args).

-spec is_bidi_prohibited(Char :: integer()) ->
    true | false.
is_bidi_prohibited(16#0340) -> true; %COMBINING GRAVE TONE MARK
is_bidi_prohibited(16#0341) -> true; %COMBINING ACUTE TONE MARK
is_bidi_prohibited(16#200E) -> true; %LEFT-TO-RIGHT MARK
is_bidi_prohibited(16#200F) -> true; %RIGHT-TO-LEFT MARK
is_bidi_prohibited(16#202A) -> true; %LEFT-TO-RIGHT EMBEDDING
is_bidi_prohibited(16#202B) -> true; %RIGHT-TO-LEFT EMBEDDING
is_bidi_prohibited(16#202C) -> true; %POP DIRECTIONAL FORMATTING
is_bidi_prohibited(16#202D) -> true; %LEFT-TO-RIGHT OVERRIDE
is_bidi_prohibited(16#202E) -> true; %RIGHT-TO-LEFT OVERRIDE
is_bidi_prohibited(16#206A) -> true; %INHIBIT SYMMETRIC SWAPPING
is_bidi_prohibited(16#206B) -> true; %ACTIVATE SYMMETRIC SWAPPING
is_bidi_prohibited(16#206C) -> true; %INHIBIT ARABIC FORM SHAPING
is_bidi_prohibited(16#206D) -> true; %ACTIVATE ARABIC FORM SHAPING
is_bidi_prohibited(16#206E) -> true; %NATIONAL DIGIT SHAPES
is_bidi_prohibited(16#206F) -> true; %NOMINAL DIGIT SHAPES
is_bidi_prohibited(_) -> false.
