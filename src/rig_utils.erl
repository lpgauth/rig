-module(rig_utils).
-include("rig.hrl").
-include_lib("kernel/include/file.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    change_time/1,
    decode_varint/1,
    encode_varint/1,
    lookup/3,
    match_all/1,
    parse_fun/1,
    read_file/5
]).

%% public
-spec change_time(file:filename()) ->
    pos_integer() | undefined.

change_time(File) ->
    case file:read_file_info(File, [raw, {time, posix}]) of
        {ok, #file_info {mtime = Mtime}} ->
            Mtime;
        {error, Reason} ->
            error_logger:warning_msg("can't read file ~p: ~p~n",
                [File, Reason]),
            undefined
    end.

-spec decode_varint(binary()) ->
    {integer(), binary()}.

decode_varint(Bytes) ->
    decode_varint(Bytes, 0, 0).

-spec encode_varint(I :: integer()) ->
    binary().

encode_varint(I) ->
    encode_varint(I, []).

-spec lookup(atom(), [{atom(), term()}], term()) ->
    term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec match_all('$end_of_table' | {[any()], ets:continuation()}) ->
    [[any()]].

match_all('$end_of_table') ->
    [];
match_all({Match, Continuation}) ->
    [Match | match_all(ets:match_object(Continuation))].

-spec parse_fun(string()) ->
    {ok, fun()} | {error, invalid_fun}.

parse_fun(Decoder) ->
    try
        {ok, Scanned, _} = erl_scan:string(Decoder),
        {ok, Parsed} = erl_parse:parse_exprs(Scanned),
        case erl_eval:exprs(Parsed, []) of
            {value, DecoderFun, _} when is_function(DecoderFun) ->
                {ok, DecoderFun};
            _ ->
                {error, invalid_fun}
        end
    catch
        _:_ ->
            {error, invalid_fun}
    end.

-spec read_file(file:io_device(), table(), decoder(), ets:tid(),
    pos_integer()) -> ok.

read_file(File, Name, Decoder, Tid, KeyElement) ->
    State = {Name, Decoder, Tid, KeyElement},
    read_file_buf(File, <<>>, 0, State).

%% private
decode_varint(<<0:1, I:7, Rest/binary>>, Int, Depth) ->
    {(I bsl Depth) bor Int, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Int, Depth) ->
    decode_varint(Rest, (I bsl Depth) bor Int, Depth + 7).

encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    LastSevenBits = (I - ((I bsr 7) bsl 7)),
    OtherBits = (I bsr 7),
    NewBit = LastSevenBits bor 16#80,
    encode_varint(OtherBits, [NewBit | Acc]).

parse_records(<<>>, 0, _State) ->
    ok;
parse_records(Bin, 0, State) when size(Bin) >= 4 ->
    {Size, Rest} = decode_varint(Bin),
    parse_records(Rest, Size, State);
parse_records(Bin, 0, _State) ->
    {Bin, 0};
parse_records(Bin, Size, State) when size(Bin) >= Size ->
    {Name, Decoder, Tid, KeyElement} = State,
    <<Record:Size/binary, Rest/binary>> = Bin,
    Record2 = case Decoder of
        term ->
            erlang:binary_to_term(Record);
        Module when is_atom(Module) ->
            Module:Name(Record);
        _ ->
            Decoder(Record)
    end,
    case Record2 of
        {Key, Value} ->
            true = ets:insert(Tid, {Key, Value});
        Tuple when is_tuple(Tuple) ->
            Key = element(KeyElement, Record2),
            true = ets:insert(Tid, {Key, Record2})
    end,
    parse_records(Rest, 0, State);
parse_records(Bin, Size, _State) ->
    {Bin, Size}.

read_file_buf(File, Buffer, Size, State) ->
    case file:read(File, ?FILE_READ_SIZE * 2) of
        eof ->
            ok;
        {ok, Bin} ->
            Bin2 = <<Buffer/binary, Bin/binary>>,
            case parse_records(Bin2, Size, State) of
                ok ->
                    read_file_buf(File, <<>>, 0, State);
                {Bin3, Size2} ->
                    read_file_buf(File, Bin3, Size2, State)
            end
    end.
