%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson).

-export([encode/1, decode/1]).

encode(Doc) when is_list(Doc) ->
    ebson_encode:document(Doc).

decode(Bin) when is_binary(Bin) ->
    ebson_decode:document(Bin).
