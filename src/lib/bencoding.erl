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
            {error, unparsed};
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
    enc(Data).

% local encode functions
enc(Int) when is_integer(Int) ->
    EncodedInt = integer_to_binary(Int),
    <<$i, EncodedInt/binary, $e>>;
enc(List) when is_list(List) ->
    EncodedList = lists:foldl(fun(Item, Acc) ->
        EncodedItem = enc(Item),
        <<EncodedItem/binary, Acc/binary>>
    end, <<>>, List),
    <<$l, EncodedList, $e>>;
enc(String) when is_binary(String) ->
    Length = integer_to_binary(byte_size(String)),
    <<Length/binary, $:, String/binary>>;
%% enc(Map) when is_map(Map) ->
%%     Map;
enc(_) ->
    exit(unknown_type).
