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
    TorrentFile = "test2.torrent",
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

    inets:stop(),

    done.

get_pieces(Info) ->
    ChunkSize0 = 1 bsl 14,
    PieceSize0 = maps:get(<<"piece length">>, Info),
    FileLength = maps:get(<<"length">>, Info),

    % function that generate list of chunks for each piece
    GenerateChunks = fun(PieceSize, ChunkSize, PieceIndex) ->
        LastChunkSize = PieceSize rem ChunkSize,
        NoOfChunks = PieceSize div ChunkSize,
        [#chunk{piece_index = PieceIndex, offset = Index * ChunkSize, size = ChunkSize} || Index <- lists:seq(0, NoOfChunks - 1)] ++
        case LastChunkSize of
            0 -> [];
            _ -> [#chunk{piece_index = PieceIndex, offset = NoOfChunks * ChunkSize, size = LastChunkSize}]
        end
    end,

    % calculate how many pieces
    LastPieceLength = FileLength rem PieceSize0,
    NoOfPieces = FileLength div PieceSize0,

    % return list of piece
    [#piece{index = Index, size = PieceSize0, offset = Index * PieceSize0, chunks = GenerateChunks(PieceSize0, ChunkSize0, Index)} || Index <- lists:seq(0, NoOfPieces - 1)] ++
    case LastPieceLength of
        0 -> [];
        _ -> [#piece{index = NoOfPieces, offset = NoOfPieces * PieceSize0, size = LastPieceLength, chunks = GenerateChunks(LastPieceLength, ChunkSize0, NoOfPieces)}]
    end.

pieces_loop(Pieces) ->
    put(binary_stored, <<>>),
    put(index, []),
    pieces_loop(Pieces, []).

pieces_loop([], []) ->
    io:format("~p~n", [get(index)]),
    file:write_file("/home/art/test.js", get(binary_stored)),
    file:write_file("/home/art/test0.js", get({index, 0})),
    io:format("Download completed ~n");
pieces_loop(Pieces, Downloading) ->
    receive
        {pop, FromPid} ->
            case Pieces of
                [] -> pieces_loop(Pieces, Downloading);
                [H | T] -> FromPid ! {piece, H#piece{status = downloading}}, pieces_loop(T, [H#piece{status = downloading} | Downloading])
            end;
        {done, FromPid, DonePiece} ->
            put(index, get(index) ++ [DonePiece#piece.index]),
            put({index, DonePiece#piece.index}, DonePiece#piece.data),
            % save data on disk or do something
            Binary = get(binary_stored),
            B = DonePiece#piece.data,
            put(binary_stored, <<Binary/binary, B/binary>>),
            UpdateDownloadList = Downloading -- [DonePiece#piece{status = downloading, peer = undefined, data = <<>>}],
            case {Pieces, UpdateDownloadList} of
                {[], []} -> FromPid ! {complete, #piece{}}, pieces_loop(Pieces, UpdateDownloadList);
                _ -> pieces_loop(Pieces, UpdateDownloadList)
            end;
        {error, ErrorPiece} ->
            pieces_loop([ErrorPiece#piece{status = undefined, peer = undefined} | Pieces], Downloading -- [ErrorPiece#piece{status = downloading, peer = undefined}])
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
    Event = "stopped",
    CloseURL = lists:flatten(io_lib:format(
        "~s?info_hash=~s&peer_id=~s&port=~p&uploaded=~p&downloaded=~p&left=~p&event=~s",
        [AnnounceUrl,InfoHash, PeerID,
            Port, Uploaded, Downloaded,Left, Event])),
    {ok, _CloseReply} = httpc:request(CloseURL),

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