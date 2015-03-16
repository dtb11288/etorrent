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
-define(M(Message), utils:message(Message)).

%% API
-export([download/1]).

download({Ip, Port}) ->
    % connect to the peer
    {ok, Sock} = gen_tcp:connect(Ip, Port,
        [binary, {packet, 0}, {active, false}]),

    % handshake with the peer
    Handshake = [19, "BitTorrent protocol", [0 || _<- lists:seq(1, 8)], get(info_hash), get(peer_id)],
    ok = gen_tcp:send(Sock, Handshake),

    % receive handshake
    {ok, <<19, "BitTorrent protocol", _:8/binary, _:20/binary, PeerID:20/binary>>} = gen_tcp:recv(Sock, 68),
    io:format("~s~n", [PeerID]),

    % pieces

    % after handshake, send choke to peer
    inet:setopts(Sock, [{packet, 4}, {active, true}]),
    gen_tcp:send(Sock, ?M(choke)),

    receive
        Any -> io:format("~p~n", [Any])
    end,

    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
%%     Peer = {"10.42.200.102", 51413},
%%     peer:download(Peer),
    done.

-endif.

