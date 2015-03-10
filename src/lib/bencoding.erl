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

decode(<<$i, Rest/binary>>) ->
    decode_integer(Rest, <<>>);
decode(<<$l, Rest/binary>>) ->
    decode_list(Rest, []);
decode(<<$d, Rest/binary>>) ->
    decode_dictionary(Rest, #{});
decode(Data) ->
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
    {R, Tail} = decode(Data),
    decode_list(Tail, [R | Result]).

decode_dictionary(<<$e, Rest/binary>>, Result) ->
    {Result, Rest};
decode_dictionary(Data, Result) ->
    {Key, TailValue} = decode(Data),
    {Value, Tail} = decode(TailValue),
    decode_dictionary(Tail, maps:put(Key, Value, Result)).

encode(_Data) ->
    [].