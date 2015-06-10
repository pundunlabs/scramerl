-module(stringprep).

%% Exported functions
-export([prepare/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Implements RFC 3454 "Preparation of Internationalized Strings
%% ("stringprep")".
%% Preperation does Mapping, Prohibiting. Currently it does not do
%% Normalization and ignores Bidirectional Characters.
%% @end
%%--------------------------------------------------------------------
-spec prepare(String :: string) -> {ok, PString :: string()} |
				   {error, Reason :: term()}.
prepare(String) ->
    case code:which(stringprep_lib) of
	non_existing ->
	    create_code();
	_ ->
	    ok
    end,
    Mapped = lists:flatten([ stringprep_lib:map(C) || C <- String ]),
    [ C || C <- Mapped, stringprep_lib:prohibit(C) == false ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates stringprep_lib module which has map and prohibit functions.
%% @end
%%--------------------------------------------------------------------
create_code()->
    PrivDir = code:priv_dir(scramerl),
    create_code(filename:join(PrivDir,"stringprep_appb.cfg"),
		filename:join(PrivDir,"stringprep_appc.cfg")).

%%--------------------------------------------------------------------
%% @doc
%% Creates stringprep_lib module which has map and prohibit functions.
%% @end
%%--------------------------------------------------------------------
create_code(MapFile, PFile) ->
    {ok, MapData} = file:read_file(MapFile),
    {ok, MapTokens, _} = erl_scan:string(binary_to_list(MapData)),
    {ok, Mappings} = stringprep_mp:parse(MapTokens),
    {ok, PData} = file:read_file(PFile),
    {ok, PTokens, _} = erl_scan:string(binary_to_list(PData)),
    {ok, PList} = stringprep_pp:parse(PTokens),
    CEForms = make_mod(Mappings, PList),
    {ok, _, Beam } = compile:forms(CEForms, [from_core, binary]),
    code:load_binary(stringprep_lib, [], Beam).

 

%%--------------------------------------------------------------------
%% @doc
%% Make module 'stringprep_lib' with map/1 and prohibit/1 functions.
%% @end
%%--------------------------------------------------------------------
make_mod(Mappings, PList) ->
    ModuleName = cerl:c_atom(stringprep_lib),
    cerl:c_module(ModuleName,
		  [cerl:c_fname(map, 1),
		   cerl:c_fname(prohibit,1),
		   cerl:c_fname(module_info, 0),
		   cerl:c_fname(module_info, 1)],
		  [make_map_fun(Mappings),
		   make_prohibit_fun(PList)] ++
		   mod_info(ModuleName)).

%%--------------------------------------------------------------------
%% @doc
%% Make map/1 function.
%% @end
%%--------------------------------------------------------------------
make_map_fun(Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_map_clauses(Arg1, Mappings),
    
    LastClause = cerl:c_clause([Else], cerl:c_atom(true), Arg1),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(map,1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for map/1 function.
%% @end
%%--------------------------------------------------------------------
make_map_clauses(Arg1, Mappings) ->
    make_map_clauses(Arg1, Mappings,[]).
    
%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for map/1 function.
%% @end
%%--------------------------------------------------------------------
make_map_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_map_clauses(Arg1, [[{integer, _Line, Val}, MapsTo ]|Rest], Acc) ->
    IntList = [cerl:c_int(Int) || {integer, _, Int} <- MapsTo],
    Clause = cerl:c_clause([cerl:c_int(Val)], cerl:c_atom(true), cerl:make_list(IntList)),
    make_map_clauses(Arg1, Rest, [Clause|Acc]);
make_map_clauses(Arg1, [[{integer, _Line, Val}]|Rest], Acc) ->
    Clause = cerl:c_clause([cerl:c_int(Val)], cerl:c_atom(true), cerl:make_list([Arg1])),
    make_map_clauses(Arg1, Rest, [Clause|Acc]);
make_map_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
    {error, io_lib:format("Syntax error: ~p", [Line])}.

%%--------------------------------------------------------------------
%% @doc
%% Make prohibit/1 function.
%% @end
%%--------------------------------------------------------------------
make_prohibit_fun(Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_prohibit_clauses(Arg1, Mappings),
    
    LastClause = cerl:c_clause([Else], cerl:c_atom(true), cerl:c_atom(false)),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(prohibit,1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for prohibit/1 function.
%% @end
%%--------------------------------------------------------------------
make_prohibit_clauses(Arg1, Mappings) ->
    make_prohibit_clauses(Arg1, Mappings,[]).
    
%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for prohibit/1 function.
%% @end
%%--------------------------------------------------------------------
make_prohibit_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_prohibit_clauses(Arg1, [[{integer, _Line, Start},{integer, _, End}]|Rest], Acc) ->
    True = cerl:c_atom(true),
    Val = cerl:c_var('Val'),
    Exp1 = make_call(erlang, '>=', [Val, cerl:c_int(Start)]),
    Exp2 = make_call(erlang, '=<', [Val, cerl:c_int(End)]),
    Exp =  make_call(erlang, 'and', [Exp1, Exp2]),
    Clause = cerl:c_clause([Val], Exp, True),
    make_prohibit_clauses(Arg1, Rest, [Clause|Acc]);
make_prohibit_clauses(Arg1, [[{integer, _Line, Val}]|Rest], Acc) ->
    True = cerl:c_atom(true),
    Clause = cerl:c_clause([cerl:c_int(Val)], True, True),
    make_prohibit_clauses(Arg1, Rest, [Clause|Acc]);
make_prohibit_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
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
