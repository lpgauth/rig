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
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

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
   io:format("starting gateway_persist"),
   
   Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
   io:format("startlink ret: ~p~n",[Ret]),
   Ret.

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

handle_info({rig_index, update, flights}, _State) ->
    {ok,Flights} = rig:all(flights),
    persist_to_cache(Flights);

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
    [extract_deal(Flight) || {_, Flight} <- Flights].

    
    
persist_to_cache(Flights) ->
    RBFlights = find_rb_flights(Flights),
    persistent_term:put({?MODULE, rb_flights}, RBFlights),
    io:format("persisted: ~p~n",[persistent_term:get({?MODULE, rb_flights})]).


