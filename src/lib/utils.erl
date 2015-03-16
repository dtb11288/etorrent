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
    <<0000>>;
message(choke) ->
    <<00010>>;
message(unchoke) ->
    <<00011>>;
message(have) ->
    <<0000>>;
message(_) ->
    throw(unknow_message).
