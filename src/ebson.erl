%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson).

-export([encode/1, decode/1, doc_size/1, has_key/2]).
-include("../include/ebson.hrl").

-spec encode(ebson_pl_doc()) -> binary().		     
encode(Doc) when is_list(Doc) ->
    ebson_encode:document(Doc).

-spec decode(binary()) -> ebson_pl_doc().		  
decode(Bin) when is_binary(Bin) ->
    ebson_decode:document(Bin).

-spec doc_size(binary()) -> integer().
doc_size(Bin) ->
    ebson_get:doc_size(Bin).

-spec has_key(binary(), binary()) -> boolean().
has_key(Key, Bin) when is_binary(Key) andalso is_binary(Bin) ->
    ebson_get:has_key(Key, Bin).
