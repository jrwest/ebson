%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_decode_SUITE).

-include_lib("common_test/include/ct.hrl").

%% common_test functions
-export([all/0, groups/0]).

%% tests
-export([
	 %% DOCUMENTS GROUP TESTS
	 empty_doc/1, 
	 one_key_doc/1,
	 three_key_doc/1,

	 %% DETECTS GROUP TESTS
	 detect_double/1, 
	 detect_string/1, 
	 detect_document/1,
	 detect_array/1,
	 detect_binary/1,
	 
	 %% VALUES GROUP TESTS
	 decode_double/1,
	 decode_string/1,
	 decode_binary/1,

	 %% NON-GROUPED TESTS
	 valid_key/1
	]).

%%%-------------------------------------------------------------------
%%% COMMON TEST FUNCTIONS
%%%-------------------------------------------------------------------
groups() ->
    [{detects,
      [shuffle],
      [detect_double, detect_string, detect_document, detect_array, detect_binary]},
     {values, 
      [shuffle],
      [decode_string, decode_double, decode_binary]},
     {documents, 
      [shuffle],
      [empty_doc, one_key_doc, three_key_doc]}].

all() ->
    [{group, detects}, {group, values}, valid_key, {group, documents}].


%%%-------------------------------------------------------------------
%%% DETECTS TESTS
%%%-------------------------------------------------------------------
detect_double(_) ->
    EncBin = <<1, 104, 104, 0, 2.23/float>>,
    float = ebson_decode:field_type(EncBin).

detect_string(_) ->
    EncBin = <<2, 104, 104, 0, 3:1/integer-little-unit:32, 104, 104, 0>>,
    string = ebson_decode:field_type(EncBin).

detect_document(_) ->
    EncBin = <<3, 104, 0, 5, 0, 0, 0, 0>>,
    document = ebson_decode:field_type(EncBin).

detect_array(_) ->
    EncBin = <<4, 104, 0, 5, 0, 0, 0, 0>>,
    array = ebson_decode:field_type(EncBin).

detect_binary(_) ->
    EncBin = <<5, 104, 0, 1, 0, 1>>,
    binary = ebson_decode:field_type(EncBin).


%%%-------------------------------------------------------------------
%%% VALUES TESTS
%%%-------------------------------------------------------------------
decode_double(_) ->
    DblBin = <<174,71,225,122,20,174,243,63>>,
    {1.23, <<>>} = ebson_decode:value(double, DblBin).

decode_string(_) ->
    StrBin = <<3:1/integer-little-unit:32, 104, 104, 0>>,
    {<<"hh">>, <<>>} = ebson_decode:value(string, StrBin).

decode_binary(_) ->
    BinBin = <<4, 0, 0, 0, 0, 1, 2, 3, 4>>, %% 0 is intentional. it is the subtype
    {<<1, 2, 3, 4>>, <<>>} = ebson_decode:value(binary, BinBin).
	

%%%-------------------------------------------------------------------
%%% DOCUMENTS TESTS
%%%-------------------------------------------------------------------
empty_doc(_Config) ->
    EncDoc = <<5, 0, 0, 0, 0>>,
    [] = ebson_decode:document(EncDoc).

one_key_doc(_) ->
    EncDoc = <<15,0,0,0,2,104,0,3,0,0,0,104,104,0,0>>,
    [{<<"h">>, <<"hh">>}] = ebson_decode:document(EncDoc).

three_key_doc(_) ->
    EncDoc = <<35,0,0,0,2,104,0,3,0,0,0,104,104,0,2,105,0,3,0,0,0,105,105,0,2,106,0,3,0,0,0,106,106,0,0>>,
    [{<<"h">>, <<"hh">>},
     {<<"i">>, <<"ii">>},
     {<<"j">>, <<"jj">>}] = ebson_decode:document(EncDoc).

%%%-------------------------------------------------------------------
%%% UNGROUPED TESTS
%%%-------------------------------------------------------------------
valid_key(_Config) ->
    EncBin = <<104, 104, 104, 0>>,
    {<<"hhh">>, <<>>} = ebson_decode:key(EncBin).


