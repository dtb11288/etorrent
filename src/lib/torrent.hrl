%%%-------------------------------------------------------------------
%%% @author art
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2015 3:03 PM
%%%-------------------------------------------------------------------
-author("art").

-record(torrent, {files}).
-record(file, {no_of_pieces, piece_size, last_piece_size}).
-record(piece, {piece_size, no_of_chunks, chunk_size, last_chunk_size}).
