%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_encode_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([demo_test/1]).

all() ->
    [demo_test].

demo_test(_C) ->
    1 = 1.

