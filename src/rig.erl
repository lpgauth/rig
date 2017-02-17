-module(rig).
-include("rig.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {rig_index_utils, tid, 1}
]).

-export([
    all/1,
    read/2,
    read/3,
    version/1
]).

%% public
-spec all(table()) ->
    {ok, [{key(), value()}]} | {error, unknown_table} .

all(Table) ->
    try
        MatchObject = ets:match_object(tid(Table), '_', 500),
        All = lists:append(rig_utils:match_all(MatchObject)),
        {ok, All}
    catch
        _:_ ->
            {error, unknown_table}
    end.

-spec read(table(), key()) ->
    {ok, value()} | {error, unknown_key | unknown_table}.

read(Table, Key) ->
    try ets:lookup(tid(Table), Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            {error, unknown_key}
    catch
        _:_ ->
            {error, unknown_table}
    end.

-spec read(table(), key(), value()) ->
    {ok, value()} | {error, unknown_table}.

read(Table, Key, Default) ->
    case read(Table, Key) of
        {error, unknown_key} ->
            {ok, Default};
        X ->
            X
    end.

-spec version(table()) ->
    {ok, ets:tid()} | {error, unknown_table}.

version(Table) ->
    try
        {ok, tid(Table)}
    catch
        _:_ ->
            {error, unknown_table}
    end.

%% private
tid(Table) ->
    case rig_index_utils:tid(Table) of
        undefined ->
            ets:lookup_element(?ETS_TABLE_INDEX, Table, 2);
        Tid ->
            Tid
    end.
