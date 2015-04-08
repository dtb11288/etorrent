%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2015 3:17 AM
%%%-------------------------------------------------------------------
-module(torrent).
-author("art").

%% API
-export([download/1, pieces_loop/1, download/0]).
-include("torrent.hrl").

download() ->
    TorrentFile = "test.torrent",
    torrent:download(TorrentFile),
    done.

download(TorrentFile) ->
    % read torrent file, decode it
    {ok, Torrent} = file:read_file(TorrentFile),
    {ok, TorrentData} = bencoding:decode(Torrent),

    % prepare information parameters
    AnnounceUrl = binary_to_list(maps:get(<<"announce">>, TorrentData)),
    PeerID = "-FT-1234567890123456",

    % info_hash
    InfoMap = maps:get(<<"info">>, TorrentData),

    {ok, Info} = bencoding:encode(maps:get(<<"info">>, TorrentData)),
    SHAInfo = binary_to_list(crypto:hash(sha, Info)),
    SHAInfoEncoded = [io_lib:format("%~2.16.0b", [X]) || X <- SHAInfo],
    put(info_hash, SHAInfo),
    put(peer_id, PeerID),
    put(info, InfoMap),

    % send request to tracker
    PeerList = tracker_connect(AnnounceUrl, SHAInfoEncoded, PeerID),

    % create pieces server
    Pieces = get_pieces(InfoMap),
    io:format("~p~n", [Pieces]),
    register(piece_list, spawn(fun() -> pieces_loop(Pieces) end)),

    % after get the peerlist, each peer in the list, connect
    % TODO: choose which peer to connect
    % TODO: now using the first peer to test
%%     io:format("~p~n", [hd(PeerList)]),
    peer:download(hd(PeerList)),

    done.

get_pieces(Info) ->
    ChunkLength = 1 bsl 14,
    PieceLength = maps:get(<<"piece length">>, Info),
    FileLength = maps:get(<<"length">>, Info),
    put(piece_length, PieceLength),
    io:format("PieceLength ~p~n", [PieceLength]),
    io:format("FileLength ~p~n", [FileLength]),

    % calculate how many pieces
    LastPieceLength = FileLength rem PieceLength,
    NoOfPieces = case LastPieceLength of
                     0 -> FileLength div PieceLength;
                     _ -> FileLength div PieceLength + 1
                 end,


    % calculate how many chunks in each piece
    LastChunkLength = PieceLength rem ChunkLength,
    NoOfChunks = case LastChunkLength of
                     0 -> PieceLength div ChunkLength;
                     _ -> PieceLength div ChunkLength + 1
                 end,


    % calculate how many chunks in last piece
    LastChunkLengthOfLastPiece = LastChunkLength rem ChunkLength,
    NoOfChunksOfLastPiece = case LastChunkLengthOfLastPiece of
                                0 -> LastPieceLength div ChunkLength;
                                _ -> LastPieceLength div ChunkLength + 1
                            end,

    % list pieces
    NormalPiece = #piece{piece_size = PieceLength, no_of_chunks = NoOfChunks, chunk_size = ChunkLength, last_chunk_size = LastChunkLength},
    LastPiece = #piece{piece_size = LastPieceLength, no_of_chunks = NoOfChunksOfLastPiece, chunk_size = ChunkLength, last_chunk_size = LastChunkLengthOfLastPiece},

    [NormalPiece || _ <- lists:seq(1, NoOfPieces - 1)] ++ [LastPiece].

pieces_loop([]) -> io:format("Download Complete~n");
pieces_loop(Pieces) ->
    io:format("Looping~n"),
    receive
        {add, Piece} -> pieces_loop([Piece | Pieces]);
        {remove, Piece} -> pieces_loop(Pieces -- [Piece])
    end.

tracker_connect(AnnounceUrl, InfoHash, PeerID) ->
    Port = 6881,
    Left = 0,
    Uploaded = 0,
    Downloaded = 0,
    Compact = 1,
    Url = lists:flatten(io_lib:format(
        "~s?info_hash=~s&peer_id=~s&port=~p&uploaded=~p&downloaded=~p&left=~p&compact=~p",
        [AnnounceUrl, InfoHash, PeerID, Port, Uploaded, Downloaded, Left, Compact]
    )),

    % get the tracker info by send GET request
    ok = inets:start(),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(Url),

    % TODO: if failed, retry, or get error message -> handle it

    % decode tracker response
    {ok, PeerInfos} = bencoding:decode(list_to_binary(Body)),
    parse_peers(maps:get(<<"peers">>, PeerInfos), []).

parse_peers(Data = <<$l, _>>, _) ->
    {ok, List} = bencoding:decode(Data),
    lists:map(fun(Map) ->
        ID = maps:get(<<"ip">>, Map),
        PortBinary = maps:get(<<"port">>, Map),
        [B1, B2] = binary_to_list(PortBinary),
        Port = B1 bsl 8 + B2,
        {ID, Port}
    end, List);
parse_peers(<<IpBinary:4/binary, PortBinary:2/binary, Rest/binary>>, PeerList) ->
    Ip = string:join([integer_to_list(X) || X <- binary_to_list(IpBinary)], "."),
    [B1, B2] = binary_to_list(PortBinary),
    Port = B1 bsl 8 + B2,
    parse_peers(Rest, [{Ip, Port} | PeerList]);
parse_peers(_, PeerList) ->
    lists:reverse(PeerList).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
    TorrentFile = "../test2.torrent",
    torrent:download(TorrentFile),
    ?assert(false),
    done.

-endif.