%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2015 9:22 AM
%%%-------------------------------------------------------------------
-module(peer).
-author("art").

%% API
-export([download/1]).

download({Ip, Port}) ->
    % connect to the peer
    {ok, Sock} = gen_tcp:connect(Ip, Port,
        [binary, {active, true}]),

    % handshake with the peer
    Handshake = "",
    ok = gen_tcp:send(Sock, Handshake),

    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
%%     Peer = {"10.42.200.102", 51413},
%%     peer:download(Peer),
    done.

-endif.

