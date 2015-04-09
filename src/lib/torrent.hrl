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
-record(piece, {index, offset, size, chunks, status, data = <<>>, peer}).
-record(chunk, {piece_index, offset, size, status}).
-record(peer, {id, ip, port}).
