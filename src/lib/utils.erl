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
-export([message/1]).

message(keep_alive) ->
    <<>>;
message(choke) ->
    <<0>>;
message(unchoke) ->
    <<1>>;
message(interested) ->
    <<2>>;
message(_) ->
    throw(unknow_message).
