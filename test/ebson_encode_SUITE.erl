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

%% common_test functions
-export([all/0, groups/0]).

%% tests
-export([
	 %% DOCUMENTS GROUP TESTS
	 empty_doc/1, 
	 one_key_doc/1,

	 %% DETECTS GROUPS TESTS
	 detect_float/1,

	 %% VALUES GROUP TESTS
	 float_val/1,

	 %% NON-GROUPED TESTS
	 binary_key/1
	]).

%%%-------------------------------------------------------------------
%%% COMMON TEST FUNCTIONS
%%%-------------------------------------------------------------------
groups() ->
    [{values,
      [shuffle],
      [float_val]},
     {detects, 
      [shuffle],
      [detect_float]},
     {documents,
      [shuffle],
      [empty_doc, one_key_doc]}].

all() ->
    [binary_key, {group, detects}, {group, values}, {group, documents}].    

%%%-------------------------------------------------------------------
%%% VALUES TESTS
%%%-------------------------------------------------------------------
float_val(_) ->
    Val = 1.23,
    <<174,71,225,122,20,174,243,63>> = ebson_encode:value(Val).

%%%-------------------------------------------------------------------
%%% DETECTS TESTS
%%%-------------------------------------------------------------------
detect_float(_) ->
    Val = 1.23,
    1 = ebson_encode:field_flag(Val).
	


%%%-------------------------------------------------------------------
%%% DOCUMENTS TESTS
%%%-------------------------------------------------------------------
empty_doc(_) ->
    Doc = [],
    <<5, 0, 0, 0, 0>> = ebson_encode:document(Doc).

one_key_doc(_) ->
    Doc = [{<<"a">>, 1.23}],
    Doc = ebson_decode:document(ebson_encode:document(Doc)).

%%%-------------------------------------------------------------------
%%% UNGROUPED TESTS
%%%-------------------------------------------------------------------
binary_key(_) ->
    Key = <<"a">>,
    <<$a, 0>> = ebson_encode:key(Key).
    
    
