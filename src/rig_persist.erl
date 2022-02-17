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
    Candidates = [Flight || {true,Flight} <- RBFlights],
    RBFlightIds = lists:sort([get_flight_id(Flight) || Flight <- Candidates]),
    ExistingRbs = persistent_term:get({?MODULE, rb_flight_ids},[]),
    case RBFlightIds == ExistingRbs of 
        true ->
            ok;
        _-> 
        persistent_term:put({?MODULE, rb_flight_ids}, RBFlightIds),
        RBMap = [#{flight_id => get_flight_id(Flight), start_date => get_start_date(Flight), 
        deals => lists:keyfind(deals, 1, Flight), countries => get_country(Flight)} || Flight <- Candidates],
        persistent_term:put({?MODULE, rb_flights}, RBMap),
        error_logger:info_msg("rb_flights: ~p~n",[persistent_term:get({?MODULE, rb_flights},[])])
    end.

get_flight_id(Flight) ->
    {_, FlightId} = lists:keyfind(flight_id,1, Flight),
    FlightId.

get_start_date(Flight) -> 
    {_, StartDate} = lists:keyfind(start_date, 1, Flight),
    StartDate.

get_country(Flight) ->
    {_,BoolExp} = lists:keyfind(boolean_expression, 1,Flight),
    parse_country(BoolExp).
    

parse_country(BoolExp) ->
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
    B1 = <<"((tvidlist_ids all of (10952,12895,23299)) and (bob) and (((not private) or (exchange = 6 and (\"HEYHAYHAY\" in deal_ids)) or (exchange = 15454 and (\"8141\" in deal_ids)))) and ((dma <> 623)) and (country = \"US\") and ((device_type_id = 4 or device_type_id = 5)))">>,
    B2 = <<"(((impression_type <> 'bob' or applist_ids one of (1387))) and (((not private) or (exchange = 15987 and (\"784\" in deal_ids)))) and (country in (\"CA\",\"US\")))">>,
    C1 = parse_country(B1),
    C2 = parse_country(B2),
    {C1, C2}.

-ifdef(EUNIT).
extract_country_test() ->
    {["US"],["CA","US"]} == testbool().



-endif.
