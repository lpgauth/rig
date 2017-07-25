-module(rig_tests).
-include("rig.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DECODER, "fun erlang:binary_to_term/1.").

rig_test() ->
    error_logger:tty(false),
    register(rig_test, self()),
    Count = length(ets:all()),

    {error, unknown_table} = rig:read(domains, 1),
    {error, unknown_table} = rig:read(domains, 1, undefined),

    application:load(?APP),
    application:set_env(?APP, configs, [
        {creatives, "./test/files/creatives.bert", term, []},
        {"./test/files/", [
            {domains, "domains.bert", term,
                [{key_element, 2}, {subscribers, [rig_test]}]},
            {invalid_fun, "invalid.bert", "my_fun:decode/1.", []}
        ]},
        {invalid_file, "", ?DECODER, []},
        {users, "./test/files/users.bert", ?DECODER, [{key_element, 2}]}
    ]),
    application:set_env(?APP, reload_delay, 50),
    {ok, _} = rig_app:start(),

    encode_bert_configs(),
    encode_bert_configs(),
    encode_bert_configs(),

    receive {rig_index, update, domains} ->
        ok
    end,
    Count = length(ets:all()) - 4,

    {ok, {domain, 1 , <<"adgear.com">>}} = rig:read(domains, 1),
    {ok, {domain, 1 , <<"adgear.com">>}} = rig:read(domains, 1, undefined),

    {error, unknown_key} = rig:read(domains, 6),
    {ok, undefined} = rig:read(domains, 6, undefined),

    {ok, {user, 5, super_admin, <<"hello3">>}} = rig:read(users, 5),
    {ok, [
        {1, {user, 1, lpgauth, <<"hello">>}},
        {3, {user, 3, root, <<"hello2">>}},
        {5, {user, 5, super_admin, <<"hello3">>}}
    ]} = rig:all(users),
    {error, unknown_table} = rig:all(invalid),

    {ok, _Tid} = rig:version(creatives),
    {error, unknown_table} = rig:version(invalid),

    rig_app:stop(),
    Count = length(ets:all()).

%% private
encode_bert_configs() ->
    [to_bert(Filename) || Filename <- filelib:wildcard("./test/files/*.term")].

to_bert(Filename) ->
    Rootname = filename:rootname(Filename),
    New = Rootname ++ ".bert",
    {ok, File} = file:open(New, [binary, raw, write]),
    {ok, Term} = file:consult(Filename),
    file_write(Term, File),
    file:close(File).

file_write([], _File) ->
    ok;
file_write([Line | T], File) ->
    Line2 = term_to_binary(Line),
    Size = rig_utils:encode_varint(size(Line2)),
    ok = file:write(File, [Size, Line2]),
    file_write(T, File).
