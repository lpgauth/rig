-module(rig).
-include("rig.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {rig_index_foil, lookup, 1}
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
    case tid(Table) of
        {ok, Tid} ->
            MatchObject = ets:match_object(Tid, '_', 500),
            All = lists:append(rig_utils:match_all(MatchObject)),
            {ok, All};
        {error, Reason}->
            {error, Reason}
    end.

-spec read(table(), key()) ->
    {ok, value()} | {error, unknown_key | unknown_table}.

read(Table, Key) ->
    case tid(Table) of
        {ok, Tid} ->
            case ets:lookup(Tid, Key) of
                [{Key, Value}] ->
                    {ok, Value};
                [] ->
                    {error, unknown_key}
            end;
        {error, Reason} ->
            {error, Reason}
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
    tid(Table).

%% private
tid(Table) ->
    try rig_index_foil:lookup(Table) of
        {ok, Tid} ->
            {ok, Tid};
        {error, key_not_found} ->
            {error, unknown_table}
    catch
        _:_ ->
            {error, unknown_table}
    end.
