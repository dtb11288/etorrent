%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2015 3:03 PM
%%%-------------------------------------------------------------------
-author("art").

-record(torrent, {announce, files, status}).
-record(file, {size, path, pieces, status}).
-record(piece, {index, size, chunks, status, data = <<>>, peer}).
-record(chunk, {index, size, status}).
-record(peer, {id, ip, port}).
