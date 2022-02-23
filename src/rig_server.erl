-module(rig_server).
-include("rig.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start_link/0
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    configs      :: [config()],
    reload_delay :: pos_integer(),
    timer_ref    :: undefined | reference(),
    tids         :: #{},
    timestamps   :: #{}
}).

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    metal:start_link(?SERVER, ?SERVER, undefined).

%% metal callbacks
-spec init(atom(), pid(), term()) ->
    {ok, term()}.

init(_Name, _Parent, undefined) ->
    Configs = ?GET_ENV(configs, ?DEFAULT_CONFIGS),
    ReloadDelay = ?GET_ENV(reload_delay, ?DEFAULT_RELOAD_DELAY),
    Configs2 = configs_validate(Configs),

    {Configs3, Timestamps} = configs_changed(Configs2, #{}),
    Tids = configs_reload(Configs3, #{}),

    {ok, #state {
        configs = Configs2,
        reload_delay = ReloadDelay,
        tids = Tids,
        timer_ref = new_timer(ReloadDelay, ?MSG_RELOAD),
        timestamps = Timestamps
    }}.

-spec handle_msg(term(), term()) ->
    {ok, term()}.

handle_msg(?MSG_RELOAD, #state {
        reload_delay = ReloadDelay,
        timestamps = Timestamp,
        configs = Configs
    } = State) ->

    {Configs2, Timestamp2} = configs_changed(Configs, Timestamp),
    [?SERVER ! {?MSG_RELOAD_CONFIG, Config} || Config <- Configs2],

    {ok, State#state {
        timestamps = Timestamp2,
        timer_ref = new_timer(ReloadDelay, ?MSG_RELOAD)
    }};
handle_msg({?MSG_RELOAD_CONFIG, {Name, _, _, _} = Config}, #state {
        tids = Tids
    } = State) ->

    {Current, New, Tids2} = new_table(Name, Tids),
    async_reload(Config, Current, New),

    {ok, State#state {
        tids = Tids2
    }}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, _State) ->
    ok.

%% private
async_reload(Config, Current, New) ->
    spawn(fun () ->
        reload(Config, Current, New)
    end).

cleanup_table(undefined) ->
    ok;
cleanup_table(Tid) ->
    ets:delete(Tid).

configs_changed(Configs, Files) ->
    configs_changed(Configs, Files, []).

configs_changed([], Timestamps, Acc) ->
    {lists:reverse(Acc), Timestamps};
configs_changed([{Name, Filename, _, _} = Index | T], Timestamps, Acc) ->
    case rig_utils:change_time(Filename) of
        undefined ->
            configs_changed(T, Timestamps, Acc);
        Timestamp ->
            case maps:get(Name, Timestamps, 0) of
                X when X >= Timestamp ->
                    configs_changed(T, Timestamps, Acc);
                _ ->
                    Files2 = maps:put(Name, Timestamp, Timestamps),
                    configs_changed(T, Files2, [Index | Acc])
            end
    end.

configs_reload([], Tids2) ->
    Tids2;
configs_reload([{Name, _, _, _} = Config | T], Tids) ->
    {Current, New, Tids2} = new_table(Name, Tids),
    reload(Config, Current, New),
    configs_reload(T, Tids2).

configs_validate(Configs) ->
    configs_validate(Configs, []).

configs_validate([], Acc) ->
    lists:flatten(lists:reverse(Acc));
configs_validate([{BaseDir, Configs} | T], Acc) ->
    Configs2 = [{Table, BaseDir ++ Filename, DecoderFun, Options} ||
        {Table, Filename, DecoderFun, Options} <- Configs],
    configs_validate(T, [configs_validate(Configs2) | Acc]);
configs_validate([{Table, Filename, term, Options} | T], Acc) ->
    DecoderFun = fun erlang:binary_to_term/1,
    configs_validate(T, [{Table, Filename, DecoderFun, Options} | Acc]);
configs_validate([{Table, Filename, {Module, Function}, Options} | T], Acc) ->
    DecoderFun = fun Module:Function/1,
    configs_validate(T, [{Table, Filename, DecoderFun, Options} | Acc]);

configs_validate([{Table, Filename, Decoder, Options} | T], Acc) ->
    case rig_utils:parse_fun(Decoder) of
        {ok, DecoderFun} ->
            configs_validate(T,
                [{Table, Filename, DecoderFun, Options} | Acc]);
        {error, invalid_fun} ->
            configs_validate(T, Acc)
    end.

new_table(Name, Tids) ->
    New = ets:new(table, [public, {read_concurrency, true}]),
    {Current, Generations} = case maps:get(Name, Tids, []) of
        [] ->
            {undefined, [New]};
        [T1] ->
            {undefined, [T1, New]};
        [T1, T2] ->
            {T1, [T2, New]}
    end,
    Tids2 = maps:put(Name, Generations, Tids),
    {Current, New, Tids2}.

new_timer(Delay, Msg) ->
    new_timer(Delay, Msg, self()).

new_timer(Delay, Msg, Pid) ->
    erlang:send_after(Delay, Pid, Msg).

reload({Name, Filename, DecoderFun, Opts}, Current, New) ->
    try
        Timestamp = os:timestamp(),
        {ok, File} = file:open(Filename, [binary, read]),
        KeyElement = ?LOOKUP(key_element, Opts, ?DEFAULT_KEY_ELEMENT),
        ok = rig_utils:read_file(File, DecoderFun, New, KeyElement),
        ok = file:close(File),
        ok = rig_index:add(Name, New),
        Subscribers = ?LOOKUP(subscribers, Opts, ?DEFAULT_SUBSCRIBERS),
        [Pid ! {rig_index, update, Name} || Pid <- Subscribers],
        cleanup_table(Current),
        Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
        error_logger:info_msg("~p config reloaded in ~p ms", [Name, Diff])
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            error_logger:error_msg("error loading ~p: ~p:~p~n~p~n",
                [Name, E, R, ?GET_STACK(Stacktrace)])
    end.