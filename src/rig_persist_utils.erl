-module(rig_persist_utils).

-include("rig.hrl").

-export([to_persistent_term/1, find_records/1]).

-define(PERSIST_CRITERIA, application:get_env(?APP, persist_criteria, [])).
-define(SELECTFIELD, get_env_value(select_field)).
-define(SELECTCRITERIA, get_env_value(select_criteria)).
-define(BOOL_KEY, get_env_value(boolean_key)).
-define(PERSIST_MAP_NAME, get_env_value(persist_map_name)).
-define(PERSIST_LIST_NAME, get_env_value(persist_list_name)).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

extract_record(Record) ->
    Match = lists:keyfind(?SELECTFIELD, 1, Record),
    Found =
        case Match of
            false ->
                false;
            {_, []} ->
                false;
            {_, Recs} ->
                Any = [lists:any(fun(RecId) -> lists:member(RecId, ?SELECTCRITERIA) end, RecIds)
                       || {_, RecIds} <- Recs],
                lists:member(true, Any);
            _ ->
                false
        end,
    {Found, Record}.

find_records(Records) ->
    Fs = [extract_record(Record) || {_, Record} <- Records, Record =/= []],
    {L1, _L2} = lists:partition(fun({A, _}) -> A == true end, Fs),
    L1.

to_persistent_term(Candidates) ->
    CandidateIds =
        lists:sort([get_id(Candidate) || Candidate <- Candidates, Candidate =/= []]),
    PrevIds = persistent_term:get({?MODULE, ?PERSIST_LIST_NAME}, []),
    persist(CandidateIds, PrevIds, Candidates).

persist(RecIds, RecIds, _) ->
    ok;
persist([], _, _) ->
    persistent_term:put({?MODULE, ?PERSIST_LIST_NAME}, []),
    persistent_term:put({?MODULE, ?PERSIST_MAP_NAME}, []);
persist(RecIds, _, Recs) ->
    PMap = build_persist_map(Recs),
    persistent_term:put({?MODULE, ?PERSIST_LIST_NAME}, RecIds),
    persistent_term:put({?MODULE, ?PERSIST_MAP_NAME}, PMap),
    error_logger:info_msg("persist_map: ~p~n",
                          [persistent_term:get({?MODULE, ?PERSIST_MAP_NAME}, [])]).

get_field_value(Field, Record) ->
    {_, Value} = lists:keyfind(Field, 1, Record),
    Value.

get_field_values(Record) ->
    FirstList =
        [{Field, get_field_value(Field, Record)} || Field <- get_env_value(map_fields)],
    BoolSearch = value_from_boolean(Record),
    maps:from_list(
        lists:append([{get_env_value(bool_field_name), BoolSearch}], FirstList)).

build_persist_map(Candidates) ->
    [get_field_values(Candidate) || Candidate <- Candidates, Candidate =/= []].

get_id(Record) ->
    {_, Id} = lists:keyfind(get_env_value(key_field), 1, Record),
    Id.

%%%% BOOLEAN STUFF %%%%%%%%%%
value_from_boolean(Record) ->
    {_, BoolExp} = lists:keyfind(?BOOL_KEY, 1, Record),
    parse_binary_string(BoolExp).

parse_binary_string(BoolExp) ->
    Split = binary:split(BoolExp, [get_env_value(b_search_key)]),
    secondsplits(Split, [BoolExp]).

secondsplits(Exp, Exp) ->
    [get_env_value(default_value)];
secondsplits([_, AfterSrchKey], _Exp) ->
    Split = binary:split(AfterSrchKey, [<<"and">>]),
    andsplits(Split, [AfterSrchKey]).

andsplits(AfterSrchKey, AfterSrchKey) ->
    [ToSplit] = AfterSrchKey,
    {ok, Tokens, _} = erl_scan:string(binary_to_list(ToSplit)),
    extract_value(Tokens);
andsplits([BfreAnd, _], _) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(BfreAnd)),
    extract_value(Tokens).

extract_value(Tokens) ->
    [C || {_, _, C} = Token <- Tokens, element(1, Token) == string].

%%%%%%% UTILS %%%%%%%%%%%%%%%%
get_env_value(Value) ->
    {_, Val} = lists:keyfind(Value, 1, ?PERSIST_CRITERIA),
    Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%EUNIT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(EUNIT).

extract_i_partied_in_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    {B1, B2} = get_buls(),
    C1 = parse_binary_string(B1),
    C2 = parse_binary_string(B2),
    {C1, C2} = {["US"], ["CA", "US"]}.

to_persistent_term_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    Candidates = get_record(),
    AMap = build_persist_map(Candidates),
    ?assertEqual([#{i_partied_in => ["DE", "FR"],
                    nonsense => [{6, [<<"IAMTHEONE">>]}],
                    got_your_number => 1,
                    some_date => {{2022, 2, 21}, {18, 0, 0}}}],
                 AMap).

get_record() ->
    [[{got_your_number, 1},
      {some_date, {{2022, 2, 21}, {18, 0, 0}}},
      {nonsense, [{6, [<<"IAMTHEONE">>]}]},
      {some_bul,
       <<"((((isthisok=true and (\"IAMTHEONE\" in nonsense)))) and (i_partied_in in (\"DE\",\"FR\")) and ((within_months of (\"1\", \"3\")))">>}]].

get_buls() ->
    {<<"((number_of_countries_i_visited = 34) and (bob = somebloke) and (i_am_number= 1 and (\"THISISNOTNONSENSE\" in nonsense)) or (i_am_number = 6 and (\"IAMBESTEST\" in nonsense)) and (i_partied_in = \"US\") and ((my_phone_is = iphone)))">>,
     <<"(((good_impression <> 'bob' or his_rating one of (worst))) and (((i_am_not_a_number = true and (\"IAMTHEONE\" in nonsense)))) and (i_partied_in in (\"CA\",\"US\")))">>}.

test_criteria() ->
    [{select_criteria, [<<"IAMBESTEST">>, <<"IAMTHEONE">>]},
     {b_search_key, <<"i_partied_in">>},
     {select_field, nonsense},
     {map_fields, [got_your_number, nonsense, some_date]},
     {key_field, got_your_number},
     {persist_map_name, my_travels},
     {persist_list_name, my_fake_ids},
     {default_value, <<"US">>},
     {boolean_key, some_bul},
     {bool_field_name, i_partied_in}].

-endif.