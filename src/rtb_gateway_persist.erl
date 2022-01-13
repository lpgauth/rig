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

-ifdef(EUNIT).
filter_test() ->
    meck:new(rtb_gateway_config, []),
    Flights = get_test_flights(),
    persist_to_cache(Flights),
    RBFlights = persistent_term:get({?MODULE, rb_flights}, undefined),
    meck:unload(rtb_gateway_config),
    persistent_term:erase({?MODULE, rb_flights}),
    Expected = [
        {[{6, [<<"ADG10288SAM">>]}], [
            {flight_id, 240391},
            {deals, [{6, [<<"ADG10288SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {[{6, [<<"ADG10331SAM">>]}], [
            {flight_id, 243741},
            {deals, [{6, [<<"ADG10331SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {[{6, [<<"ADG10343SAM">>]}], [
            {flight_id, 243208},
            {deals, [{6, [<<"ADG10343SAM">>]}]},
            {campaign_id, 77767},
            {buyer_id, 11}
        ]}
    ],
    ?assertEqual(Expected, RBFlights).

get_test_flights() ->
    [
        {239090, [
            {flight_id, 239090},
            %%priority 2, should be in the list
            {deals, [{6, [<<"ADG10345SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 314}
        ]},
        {243224, [
            {flight_id, 243224},
            {campaign_id, 77769},
            {deals, [{7, [<<"1R-RX1-97931-20200331744602">>]}]}
        ]},
        {243225, [
            {flight_id, 243225},
            {campaign_id, 77711},
            {deals, [{7, [<<"1R-RX1-97931-20200331744602">>]}]}
        ]},
        {240391, [
            {flight_id, 240391},
            {deals, [{6, [<<"ADG10288SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {244093, [
            {flight_id, 244093},
            {deals, [{6, [<<"ADG10457SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {243741, [
            {flight_id, 243741},
            %priority 2
            {deals, [{6, [<<"ADG10331SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {234702, [
            {flight_id, 234702},
            %same campaign as 234702 which is priority 2.
            {deals, [{6, [<<"ADG10349SAM">>]}]},
            {campaign_id, 77464},
            {buyer_id, 11}
        ]},
        {242934, [
            {flight_id, 242934},
            % priority 2
            {deals, [{6, [<<"ADG10363TAF">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {244778, [
            {flight_id, 244778},
            % priority 1
            {deals, [{6, [<<"ADG10347SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {243220, [
            {flight_id, 243220},
            %priority 5
            {deals, [{6, [<<"ADG10348SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {243208, [
            {flight_id, 243208},
            %priority 8
            {deals, [{6, [<<"ADG10343SAM">>]}]},
            {campaign_id, 77767},
            {buyer_id, 11}
        ]},
        {244777, [
            {flight_id, 244777},
            {campaign_id, 77661},
            {deals, [{6, [<<"ADG10362TAS">>]}]}
        ]}
    ].

get_all_flights() ->
    [
        {239090,

        [
            {flight_id, 239090},
            %%priority 2, should be in the list
            {deals, [{6, [<<"ADG10345SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 314}
        ]},
        {243224, [
            {flight_id, 243224},
            {campaign_id, 77769},
            {deals, [{7, [<<"1R-RX1-97931-20200331744602">>]}]}
        ]},
        {243225, [
            {flight_id, 243225},
            {campaign_id, 77711},
            {deals, [{7, [<<"1R-RX1-97931-20200331744602">>]}]}
        ]},
        {240391, [
            {flight_id, 240391},
            {deals, [{6, [<<"ADG10288SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {244093, [
            {flight_id, 244093},
            {deals, [{6, [<<"ADG10457SAM">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {243741, [
            {flight_id, 243741},
            %priority 2
            {deals, [{6, [<<"ADG10331SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {234702, [
            {flight_id, 234702},
            %same campaign as 234702 which is priority 2.
            {deals, [{6, [<<"ADG10349SAM">>]}]},
            {campaign_id, 77464},
            {buyer_id, 11}
        ]},
        {242934, [
            {flight_id, 242934},
            % priority 2
            {deals, [{6, [<<"ADG10363TAF">>]}]},
            {campaign_id, 77764},
            {buyer_id, 11}
        ]},
        {244778, [
            {flight_id, 244778},
            % priority 1
            {deals, [{6, [<<"ADG10347SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {243220, [
            {flight_id, 243220},
            %priority 5
            {deals, [{6, [<<"ADG10348SAM">>]}]},
            {campaign_id, 77761},
            {buyer_id, 11}
        ]},
        {243208, [
            {flight_id, 243208},
            %priority 8
            {deals, [{6, [<<"ADG10343SAM">>]}]},
            {campaign_id, 77767},
            {buyer_id, 11}
        ]},
        {244777, [
            {flight_id, 244777},
            {campaign_id, 77661},
            {deals, [{6, [<<"ADG10362TAS">>]}]}
        ]}
    ].

-endif.

