-module(rig_compiler).
-include("rig.hrl").

-export([
    index_utils/0
]).

%% public
-spec index_utils() ->
    ok.

index_utils() ->
    Forms = index_utils_forms(ets:tab2list(?ETS_TABLE_INDEX)),
    compile_and_load_forms(Forms),
    ok.

%% private
compile_and_load_forms(Forms) ->
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin),
    ok.

index_utils_forms(Indexes) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(rig_index_utils)]),
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(tid),
        erl_syntax:integer(1))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function1 = erl_syntax:function(erl_syntax:atom(tid),
        tid_clauses(Indexes, [])),
    Mod = [Module, Export, Function1],
    [erl_syntax:revert(X) || X <- Mod].

tid_clause(Name, Tid) ->
    Var1 = erl_syntax:atom(Name),
    Body = erl_syntax:integer(Tid),
    erl_syntax:clause([Var1], [], [Body]).

tid_clause_anon() ->
    Var = erl_syntax:variable("_"),
    Body = erl_syntax:atom(undefined),
    erl_syntax:clause([Var], [], [Body]).

tid_clauses([], Acc) ->
    Acc ++ [tid_clause_anon()];
tid_clauses([{Name, Tid} | T], Acc) ->
    tid_clauses(T, Acc ++ [tid_clause(Name, Tid)]).
