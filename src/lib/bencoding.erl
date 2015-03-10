%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2015 4:34 PM
%%%-------------------------------------------------------------------
-module(bencoding).
-author("art").

%% API
-export([encode/1, decode/1]).

% decode bencoding binary to erlang term (integer, list, binary, hashmap)
decode(Data) ->
    case catch dec(Data) of
        {'EXIT', _} ->
            {error, cannot_decode};
        {Result, _} ->
            {ok, Result}
    end.

% local decode functions
dec(<<$i, Rest/binary>>) ->
    decode_integer(Rest, <<>>);
dec(<<$l, Rest/binary>>) ->
    decode_list(Rest, []);
dec(<<$d, Rest/binary>>) ->
    decode_dictionary(Rest, #{});
dec(Data) ->
    decode_string(Data, <<>>).

decode_integer(<<$e, Rest/binary>>, Result) ->
    {binary_to_integer(Result), Rest};
decode_integer(<<B, Rest/binary>>, Result) ->
    decode_integer(Rest, <<Result/binary, B>>).

decode_string(<<$:, Rest/binary>>, L) ->
    Length = binary_to_integer(L),
    <<Result:Length/binary, Tail/binary>> = Rest,
    {Result, Tail};
decode_string(<<B, Rest/binary>>, L) ->
    decode_string(Rest, <<L/binary, B>>).

decode_list(<<$e, Rest/binary>>, Result) ->
    {lists:reverse(Result), Rest};
decode_list(Data, Result) ->
    {R, Tail} = dec(Data),
    decode_list(Tail, [R | Result]).

decode_dictionary(<<$e, Rest/binary>>, Result) ->
    {Result, Rest};
decode_dictionary(Data, Result) ->
    {Key, TailValue} = dec(Data),
    {Value, Tail} = dec(TailValue),
    decode_dictionary(Tail, maps:put(Key, Value, Result)).

% encode erlang term to bencoding binary
encode(Data) ->
    case catch enc(Data) of
        {'EXIT', _} ->
            {error, cannot_encode};
        Result ->
            {ok, Result}
    end.

% local encode functions
enc(Int) when is_integer(Int) ->
    EncodedInt = integer_to_binary(Int),
    <<$i, EncodedInt/binary, $e>>;
enc(List) when is_list(List) ->
    EncodedList = lists:foldl(fun(Item, Acc) ->
        EncodedItem = enc(Item),
        <<Acc/binary, EncodedItem/binary>>
    end, <<>>, List),
    <<$l, EncodedList/binary, $e>>;
enc(String) when is_binary(String) ->
    Length = integer_to_binary(byte_size(String)),
    <<Length/binary, $:, String/binary>>;
enc(Map) when is_map(Map) ->
    EncodedMap = maps:fold(fun(Key, Value, Acc) ->
        EncodedKey = enc(Key),
        EncodedValue = enc(Value),
        <<Acc/binary, EncodedKey/binary, EncodedValue/binary>>
    end, <<>>, Map),
    <<$d, EncodedMap/binary, $e>>;
enc(_) ->
    exit(unknown_type).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    ?assertEqual({ok, 123}, bencoding:decode(<<"i123e">>)),
    ?assertEqual({ok, <<"abcde">>}, bencoding:decode(<<"5:abcde">>)),
    ?assertEqual({ok, [123, <<"abcde">>]}, bencoding:decode(<<"li123e5:abcdee">>)),
    ?assertEqual({ok, #{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>}}, bencoding:decode(<<"d4:key16:value14:key26:value2e">>)),
    ?assertEqual({error, cannot_decode}, bencoding:decode(what_the_hell)),
    done.

encode_test() ->
    ?assertEqual({ok, <<"i123e">>}, bencoding:encode(123)),
    ?assertEqual({ok, <<"5:abcde">>}, bencoding:encode(<<"abcde">>)),
    ?assertEqual({ok, <<"li123e5:abcdee">>}, bencoding:encode([123, <<"abcde">>])),
    ?assertEqual({ok, <<"d4:key16:value14:key26:value2e">>}, bencoding:encode(#{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>})),
    ?assertEqual({error, cannot_encode}, bencoding:encode(what_the_hell)),
    done.

-endif.

