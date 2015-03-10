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
-export([download/1]).

download(TorrentFile) ->
    % read torrent file, decode it
    {ok, Torrent} = file:read_file(TorrentFile),
    {ok, TorrentData} = bencoding:decode(Torrent),

    % send request to tracker
    ResponseData = send_info(TorrentData),

    io:format("~p~n", [ResponseData]),

    done.

send_info(TorrentData) ->
    % prepare params
    % announce url
    AnnounceUrl = binary_to_list(maps:get(<<"announce">>, TorrentData)),
    PeerID = "-FT-1234567890123456",

    % info_hash
    {ok, Info} = bencoding:encode(maps:get(<<"info">>, TorrentData)),
    SHAInfo = binary_to_list(crypto:hash(sha, Info)),

    % get tracker response
    Body = tracker_connect(AnnounceUrl, SHAInfo, PeerID),

    % decode tracker response
    {ok, PeerInfos} = bencoding:decode(list_to_binary(Body)),
    PeerList = parse_peers(maps:get(<<"peers">>, PeerInfos), []),
    io:format("~p~n", [PeerList]),

    done.

tracker_connect(AnnounceUrl, InfoHash, PeerID) ->
    Port = 6881,
    Left = 0,
    Uploaded = 0,
    Downloaded = 0,
    Url = lists:flatten(io_lib:format(
        "~s?info_hash=~s&peer_id=~s&port=~p&uploaded=~p&downloaded=~p&left=~p&compact=1",
        [AnnounceUrl, InfoHash, PeerID, Port, Uploaded, Downloaded, Left]
    )),

    ok = inets:start(),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(Url),

    Body.

parse_peers(Data = <<$l, _>>, PeerList) ->
    {ok, List} = bencoding:decode(Data),
    [].
parse_peers(<<IpBinary:4/binary, PortBinary:2/binary, Rest/binary>>, PeerList) ->
    Ip = string:join([integer_to_list(X) || X <- binary_to_list(IpBinary)], "."),
    [B1, B2] = binary_to_list(PortBinary),
    Port = B1 * 256 + B2,
    parse_peers(Rest, [{Ip, Port} | PeerList]);
parse_peers(_, PeerList) ->
    lists:reverse(PeerList).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

download_test() ->
    TorrentFile = "../test.torrent",
    torrent:download(TorrentFile),
    ?assert(false).

-endif.