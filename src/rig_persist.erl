%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rig_persist).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

-include("rig.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([start_link/0]).
-export([testbool/0]).

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

-define(RBDEALS, rb_deals()).


-spec start_link() ->
    {ok, pid()}.
start_link() -> 
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rb_deals() ->
    ?GET_ENV(rb_deals, []).

% Callbacks
-spec init([]) ->
    {ok, term()}.
init(_Args) ->
    {ok, #state{}}.

-spec handle_call(term(), pid(),term()) ->
    {ok, term()}.
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-spec handle_cast(term(), term()) ->
    {ok, term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), term()) ->
    {ok, term()}.
handle_info({rig_index, update, flights}, State) ->
    {ok,Flights} = rig:all(flights),
    persist_to_cache(Flights),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), term()) ->
    term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(),term()) ->
    {ok, term()}.
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
    RBFlightIds = lists:sort([?LOOKUP(flight_id, Flight, undefined) || Flight <- Flights]),
    ExistingRbs = persistent_term:get({?MODULE, rb_flight_ids},[]),
    case RBFlightIds == ExistingRbs of 
        true -> ok;
        _-> 
        %persistent_term:remove({?MODULE, rb_flight_ids}),
        persistent_term:put({?MODULE, find_rb_flights}, RBFlightIds),
        RBMap = [#{flight_id => ?LOOKUP(flight_id, Flight, undefined), start_date => ?LOOKUP(startdate, Flight, undefined), 
        deals => ?LOOKUP(deals, Flight, undefined), countries => get_country(Flight)} || Flight <- RBFlights],
        %persistent_term:remove({?MODULE, rb_flights}),
        persistent_term:put({?MODULE, rb_flights}, RBMap),
        error_logger:info_msg("rb_flights: ~p~n",[persistent_term:get({?MODULE, rb_flights},[])])
    end.

get_country(BoolExp) ->
    Split = binary:split(BoolExp, [<<"country">>]),
    countrysplits(Split, [BoolExp]).
    
countrysplits(Exp, Exp) -> 
    [<<"US">>];
countrysplits([_, AfterCtry], _Exp) ->
    Split = binary:split(AfterCtry, [<<"and">>]),
    andsplits(Split, [AfterCtry]).

andsplits(AfterCtry, AfterCtry ) ->
    [ToSplit] = AfterCtry,
    {ok, Tokens, _} = erl_scan:string(binary_to_list(ToSplit)),
    extract_country(Tokens);
andsplits([BfreAnd, _], _) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(BfreAnd)),
    extract_country(Tokens).

extract_country(Tokens) ->
    [C || {_, _, C} = Token <- Tokens, element(1,Token) == string].
 
-spec testbool() -> tuple().
testbool() -> 
    B1 = <<"((tvidlist_ids all of (10952,12895,23299)) and (matched_tv) and ((impression_type <> 'app' or applist_ids none of (573,828,2435))) and (((not private) or (exchange = 6 and (\"ADG10365AEN\" in deal_ids or \"ADG10336FUN\" in deal_ids or \"ADG10322FUS\" in deal_ids or \"ADG10321JUK\" in deal_ids or \"ADG10313DIG\" in deal_ids or \"ADG10312DIG\" in deal_ids or \"ADG10314TAS\" in deal_ids or \"ADG10301OUT\" in deal_ids or \"ADG10309JUK\" in deal_ids or \"ADG10310THI\" in deal_ids or \"ADG10311DAN\" in deal_ids or \"ADG10305XUM\" in deal_ids or \"ADG10318HAY\" in deal_ids or \"ADG10319HAY\" in deal_ids or \"ADG10335FUN\" in deal_ids or \"ADG10368HAY\" in deal_ids)) or (exchange = 15 and (\"181e7.5cf31.91fb\" in deal_ids or \"3de16.2c733.3d32\" in deal_ids or \"48f74.51e52.8141\" in deal_ids)))) and ((dma <> 623 and dma <> 770 and dma <> 539 and dma <> 560 and dma <> 616 and dma <> 510)) and (country = \"US\") and ((device_type_id = 4 or device_type_id = 5))) and ((within_frequency_cap(\"campaign:ip\", \"3561301\", 3, 86400) and within_frequency_cap(\"campaign:ip\", \"3561302\", 2, 3600)))">>,
    B2 = <<"(((impression_type <> 'app' or applist_ids one of (1387))) and (((not private) or (exchange = 15 and (\"39e5b.905c9.e784\" in deal_ids)))) and (country in (\"CA\",\"US\")))">>,
    C1 = get_country(B1),
    C2 = get_country(B2),
    {C1, C2}.

-ifdef(EUNIT).
extract_country_test() ->
    {["US"],["CA","US"]} == testbool().



-endif.

