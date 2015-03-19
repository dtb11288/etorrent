%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Mar 2015 11:57 AM
%%%-------------------------------------------------------------------
-module(utils).
-author("art").

%% API
-export([message/1, integer_to_bits/1]).

message(keep_alive) ->
    <<>>;
message(choke) ->
    <<0>>;
message(unchoke) ->
    <<1>>;
message(interested) ->
    <<2>>;
message(not_interested) ->
    <<3>>;
message(_) ->
    throw(unknow_message).

integer_to_bits(Int) ->
    integer_to_bits(Int, 0, []).

integer_to_bits(_, 8, R) -> R;
integer_to_bits(Int, Cur, R) ->
    integer_to_bits(Int bsr 1, Cur + 1, [Int band 1 | R]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integer_to_bits_test() ->
    ?assertEqual([0,1,1,1,1,1,1,1], integer_to_bits(127)),
    ?assertEqual([0,0,0,0,0,0,0,0], integer_to_bits(0)),
    ?assertEqual([0,0,0,0,0,0,0,1], integer_to_bits(1)),
    ?assertEqual([1,1,1,1,1,1,1,1], integer_to_bits(255)).

-endif.
