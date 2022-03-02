-module(rig_persist_utils).

-include("rig.hrl").

-export([to_persistent_term/1]).

-define(PERSIST_CRITERIA, application:get_env(?APP, persist_criteria, [])).
-define(SELECTFIELD, get_env_value(select_field)).
-define(SELECTCRITERIA, get_env_value(select_criteria)).
-define(BOOL_KEY, get_env_value(boolean_key)).
-define(PERSIST_MAP_NAME, get_env_value(persist_map_name)).
-define(PERSIST_LIST_NAME, get_env_value(persist_list_name)).
-define(KEY_FIELD, get_env_value(key_field)).
-define(PERSIST_TERM, rig_persist).

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

-spec to_persistent_term(list()) -> ok.
to_persistent_term(Records) ->
    Candidates = get_candidates(Records),
    CandidateIds =
        lists:sort([get_id(Candidate) || Candidate <- Candidates, Candidate =/= []]),
    PrevIds = persistent_term:get({?MODULE, ?PERSIST_LIST_NAME}, []),
    %eliminate_existing_flights(CandidateIds, PrevIds),
    persist(CandidateIds, PrevIds, Candidates).

get_candidates(Records) ->
    Recs = find_records(Records),
    [Rec || {true, Rec} <- Recs].

persist(RecIds, RecIds, _) ->
    ok;
persist([], _, _) ->
    persistent_term:put({?PERSIST_TERM, ?PERSIST_LIST_NAME}, []),
    persistent_term:put({?PERSIST_TERM, ?PERSIST_MAP_NAME}, []),
    ok;
persist(RecIds, PrevIds, Recs) ->
    {RecIds, MergedMap} = prepare_to_persist(RecIds, PrevIds, Recs),
    put_to_persist(RecIds, MergedMap).

prepare_to_persist(RecIds, PrevIds, Recs) ->
    {ParseThese, DelThese, _Stays} = purge_old(RecIds,PrevIds),
    OldMapList = persistent_term:get({?MODULE, ?PERSIST_MAP_NAME}, []),
    ToBuild = get_new_recs_to_be_parsed(ParseThese, Recs),
    NewMapList = build_persist_map(ToBuild),
    PurgedMapList = purge_old_map(DelThese, OldMapList),
    MergedMap = lists:append(PurgedMapList, NewMapList),
    {RecIds, MergedMap}.

put_to_persist(RecIds, MergedMap) ->
    persistent_term:put({?PERSIST_TERM, ?PERSIST_LIST_NAME}, RecIds),
    persistent_term:put({?PERSIST_TERM, ?PERSIST_MAP_NAME}, MergedMap),
    error_logger:info_msg("persist_map length: ~p~n",
        [length(RecIds)]),
    ok.

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
get_new_recs_to_be_parsed(ParseThese, Recs) ->
    [Rec || Rec <- Recs, lists:member(get_id(Rec), ParseThese)].

purge_old_map(DelThese, OldMapList) ->
    Purges = lists:filter(fun(Map) ->  lists:member(maps:get(?KEY_FIELD, Map), DelThese) end, OldMapList),
    OldMapList -- Purges.

purge_old(N, P) ->
   NN = N -- P,
   D = P -- N,
   S = P -- D,
   {NN, D, S}.

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

preopare_to_persist_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    OldMapList = persistent_term:put({?MODULE, ?PERSIST_MAP_NAME}, map1()),
    NewIds = get_new(),
    OldIds = get_old(),
    Recs = recs(),
    {NewIds, MergedMap} = prepare_to_persist(NewIds, OldIds, Recs),
    ?assertEqual(length(MergedMap) , 8).

purge_old_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    New = get_new(),
    Old = get_old(),
    Expected = {[297536],[297814], get_stays()},
    Purges = purge_old(New,Old),
    ?assertEqual(Purges, Expected).

purge_old_map_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    DelThese = [297814],
    OldMapList = old_map_list(),
    NewMap = purge_old_map(DelThese, OldMapList),
    ?assertEqual(NewMap, stays()).

get_new_recs_to_be_parsed_test() ->
    application:set_env(?APP, persist_criteria, test_criteria()),
    TBeParsed = [297536],
    Recs = recs(),
    Which = get_new_recs_to_be_parsed(TBeParsed, Recs),
    Expected = record_to_be_parsed(),
    ?assertEqual(Which, Expected).

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

old_map_list() ->
    [#{i_partied_in => ["CA"],
            nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 297814,
            some_date => {{2021,12,7},{11,0,0}}},
        #{i_partied_in => ["US"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 297537,
            some_date => {{2022,2,23},{21,0,0}}},
        #{i_partied_in => ["GB"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 296402,
            some_date => {{2022,3,3},{17,0,0}}}].

stays() ->
    [#{i_partied_in => ["US"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 297537,
            some_date => {{2022,2,23},{21,0,0}}},
        #{i_partied_in => ["GB"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 296402,
            some_date => {{2022,3,3},{17,0,0}}}].

new_record_parsed() ->
[#{i_partied_in => ["US"],
  nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 297536,
            some_date => {{2022,2,23},{20,0,0}}}].

record_to_be_parsed() ->
    [[{got_your_number,297536},
    {some_date,{{2022,2,23},{20,0,0}}},
    {nonsense,[{6,[<<"IAMTHEONE">>]}]},
    {some_bul,<<"((((not private) or (mynumber = 6 and (\"IAMTHEONE\" in nonsense)))) and (i_partied_in = \"US\") and (whichyear in ('2021_OSCARS')))">>}
    ]].

recs() ->
    [[{got_your_number,297536},
    {some_date,{{2022,2,23},{20,0,0}}},
    {nonsense,[{6,[<<"IAMTHEONE">>]}]},
    {some_bul,<<"((((not private) or (mynumber = 6 and (\"IAMTHEONE\" in nonsense)))) and (i_partied_in = \"US\") and (whichyear in ('2021_OSCARS')))">>}
    ],
    [{got_your_number,211226},
    {some_date,{{2022,3,23},{20,0,0}}},
    {nonsense,[{6,[<<"IAMTHEBESTEST">>]}]},
    {some_bul,<<"((((not private) or (mynumber = 6 and (\"IAMTHEONE\" in nonsense)))) and (i_partied_in = \"DE\") and (whichyear in ('2021_OSCARS')))">>}
    ],
    [{got_your_number,249536},
    {some_date,{{2022,2,23},{20,0,0}}},
    {nonsense,[{6,[<<"THISISNOTNONSENSE">>]}]},
    {some_bul,<<"((((not private) or (mynumber = 6 and (\"THISISNOTNONSENSE\" in nonsense)))) and (i_partied_in = \"US\") and (whichyear in ('2021_OSCARS')))">>}
    ]].


map1() ->
    [#{i_partied_in => ["US"],
            nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 297536,
            some_date => {{2020,10,6},{17,0,0}}},
        #{i_partied_in => ["DE"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 211224,
            some_date => {{2022,3,31},{4,0,0}}},
        #{i_partied_in => ["KR"],
            nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 296472,
            some_date => {{2022,2,26},{9,0,0}}},
        #{i_partied_in => ["KR"],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 297814,
            some_date => {{2021,8,26},{1,0,0}}},
        #{i_partied_in => ["AU"],
            nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 297376,
            some_date => {{2022,2,26},{7,0,0}}},
        #{i_partied_in => [<<"US">>],
            nonsense => [{6,[<<"IAMBESTEST">>]}],
            got_your_number => 170836,
            some_date => {{2020,10,12},{21,0,0}}},
        #{i_partied_in => ["MX"],
            nonsense => [{6,[<<"ADG10331SAM">>]}],
            got_your_number => 296017,
            some_date => {{2022,2,11},{23,45,30}}},
        #{i_partied_in => ["GB"],
            nonsense => [{6,[<<"IAMTHEONE">>]}],
            got_your_number => 296402,
            some_date => {{2022,3,3},{17,0,0}}}].



get_new() ->
    [170836,211224,249340,296017,296402,296472,297376,297536].

get_old() ->
    [170836,211224,249340,296017,296402,296472,297376,297814].

get_stays() ->
    [170836,211224,249340,296017,296402,296472,297376].

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
