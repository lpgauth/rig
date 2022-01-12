%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rtb_gateway_persist).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

-include("rig.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2, test/0]).

% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

% API

%    <<"ADG10288SAM">>, P1
%    <<"ADG10331SAM">>, P2
%    <<"ADG10342SAM">>, P3
%    <<"ADG10343SAM">>, P4
%    <<"ADG10344SAM">>, P5

-define(RBDEALS, rb_deals()).

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->  
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rb_deals() ->
    ?GET_ENV(rb_deals, []).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

% Callbacks

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rig_index, update, flights}, State) ->
    io:format("rig index update"),
    {ok,Flights} = rig:all(flights),
    persist_to_cache(Flights),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

extract_deal(Flight) ->
    %{deals,[{6,[<<"DEAL1">>]}]}
    Match = lists:keyfind(deals, 1, Flight),
    Rb = case Match of false -> false;
    _-> {deals, Deals} = Match,
        lists:any(fun({_,[DealId]})-> 
        lists:member(DealId, ?RBDEALS) end, Deals)
    end,
    {Rb, Flight}.


find_rb_flights(Flights) ->
    Fs = [extract_deal(Flight) || {_, Flight} <- Flights],
    {L1, _L2} = lists:partition(fun({A, _}) -> A == true end, Fs),
    L1.
   
persist_to_cache(Flights) ->
    RBFlights = find_rb_flights(Flights),
    persistent_term:put({?MODULE, rb_flights}, RBFlights),
    io:format("persisted: ~p~n",[persistent_term:get({?MODULE, rb_flights})]).



test() ->
    try
        Timestamp = os:timestamp(),
        DecoderFun = fun erlang:binary_to_term/1,
        {ok, File} = file:open("priv/bertconfs/development/flights.bert2", [binary, read]),
        New = ets:new(flights, [public, {read_concurrency, true}]),
        ok = rig_utils:read_file(File, DecoderFun, New, 1),
        ok = file:close(File),
        ok = rig_index:add(flights, New),
        %Subscribers = ?LOOKUP(subscribers, Opts, ?DEFAULT_SUBSCRIBERS),
        %io:format("Subscribers: ~p~n",[Subscribers]),
        rtb_gateway_persist ! {rig_index, update, flights},
        Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
        io:format("~p config reloaded in ~p ms", [flights, Diff])
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            io:format("error loading ~p: ~p:~p~n~p~n",
                [flights, E, R, ?GET_STACK(Stacktrace)])
    end.
