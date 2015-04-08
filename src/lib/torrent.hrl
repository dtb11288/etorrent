%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2015 3:03 PM
%%%-------------------------------------------------------------------
-author("art").

-record(torrent, {announce, files}).
-record(file, {size, path, pieces}).
-record(piece, {size, chunks}).
-record(chunk, {size}).
-record(peer, {id, ip, port}).
