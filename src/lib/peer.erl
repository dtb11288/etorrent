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
-include("torrent.hrl").

%% API
-export([download/1]).

download({Ip, Port}) ->
    io:format("~p~n", [{Ip, Port}]),
    % connect to the peer
    {ok, Sock} = gen_tcp:connect(Ip, Port,
        [binary, {packet, 0}, {active, false}]),

    % handshake with the peer
    Handshake = list_to_binary([19, "BitTorrent protocol", [0 || _<- lists:seq(1, 8)], get(info_hash), get(peer_id)]),
    ok = gen_tcp:send(Sock, Handshake),

    % receive handshake
    {ok, <<19, "BitTorrent protocol", _:8/binary, _:20/binary, PeerID:20/binary>>} = gen_tcp:recv(Sock, 68),
    io:format("Client PeerID ~s~n", [PeerID]),

    % after handshake, communicate with peer
    inet:setopts(Sock, [{packet, 4}, {active, true}]),

    % receive bitfield
    receive
        {tcp, _, BitField} -> io:format("~w~n", [BitField])
    after 10000 ->
        io:format("Timeout")
    end,

    %% send interest message
    ok = gen_tcp:send(Sock, ?M(interested)),
    receive
        {tcp, _, Answer} -> io:format("~w~n", [Answer])
    after 10000 ->
        % if you dont receive choked message (or any message), just keep continue request
        io:format("Timeout 2")
    end,

    download_loop(Sock),
    gen_tcp:close(Sock),
    ok.

download_loop(Sock) ->
    Self = self(),
    % popout a piece and download it
    piece_list ! {pop, Self},
    receive
        {complete, #piece{}} -> download_completed;
        {piece, Piece} ->
            case piece:download(Piece#piece{peer = Sock})of
                {ok, DonePiece} ->
                    piece_list ! {done, Self, DonePiece},
                    download_loop(Sock);
                {error, ErrorPiece} ->
                    piece_list ! {error, ErrorPiece}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
%%     Peer = {"10.42.200.102", 51413},
%%     peer:download(Peer),
    done.

-endif.

