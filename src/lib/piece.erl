%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2015 2:14 PM
%%%-------------------------------------------------------------------
-module(piece).
-author("art").
-include("torrent.hrl").

%% API
-export([download/1]).

download(Piece = #piece{index = _, chunks = Chunks, size = _PieceSize, status = _Status, peer = Socket}) ->
    case chunks_loop(Chunks, Socket, <<>>) of
        {ok, Binary} -> {ok, Piece#piece{status = done, data = Binary}};
        _ -> {error, Piece}
    end.

chunks_loop([], _Socket, Binary) ->
    {ok, Binary};
chunks_loop([#chunk{index = Index, size = Size, status = _} | RestChunks], Socket, Binary) ->
    ok = gen_tcp:send(Socket, binary_to_list(utils:request(
        <<Index:32>>,
        <<0:32>>,
        <<Size:32>>)
    )),
    receive
        {tcp, _, Downloaded} -> chunks_loop(RestChunks, Socket, <<Binary/binary, Downloaded/binary>>)
    after 30000 ->
        io:format("Problem when download chunk"),
        {error, cannot_download_chunk}
    end.





