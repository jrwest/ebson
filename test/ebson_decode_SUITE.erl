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
	 complex_doc/1,

	 %% DETECTS GROUP TESTS
	 detect_double/1, 
	 detect_string/1, 
	 detect_document/1,
	 detect_array/1,
	 detect_binary/1,
	 detect_bool/1,
	 detect_utc/1,
	 detect_null/1,
	 detect_int32/1,
	 detect_int64/1,
	 
	 %% VALUES GROUP TESTS
	 decode_double/1,
	 decode_string/1,
	 decode_binary/1,
	 decode_bool_t/1,
	 decode_bool_f/1,
	 decode_utc/1,
	 decode_null/1,
	 decode_int32/1,
	 decode_int64/1,
	 decode_document/1,
	 decode_array/1,

	 %% NON-GROUPED TESTS
	 valid_key/1
	]).

%%%-------------------------------------------------------------------
%%% COMMON TEST FUNCTIONS
%%%-------------------------------------------------------------------
groups() ->
    [{detects,
      [shuffle],
      [detect_double, detect_string, detect_document, 
       detect_array, detect_binary, detect_bool, 
       detect_utc, detect_null, detect_int32, detect_int64]},
     {values, 
      [shuffle],
      [decode_string, decode_double, decode_binary, 
       decode_bool_t, decode_bool_f, decode_utc,
       decode_null, decode_int32, decode_int64, 
       decode_document, decode_array]},
     {documents, 
      [shuffle],
      [empty_doc, one_key_doc, three_key_doc, complex_doc]}].

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

detect_bool(_) ->
    EncBin = <<8, 104, 0, 0>>,
    bool = ebson_decode:field_type(EncBin).

detect_utc(_) ->
    EncBin = <<9, 104, 0, 1, 2, 3, 4, 5, 6, 7, 8>>,
    unix_time = ebson_decode:field_type(EncBin).

detect_null(_) ->
    EncBin = <<10, 104, 0>>,
    null = ebson_decode:field_type(EncBin).

detect_int32(_) ->
    EncBin = <<16, 104, 0, 1, 0, 0, 0>>,
    int32 = ebson_decode:field_type(EncBin).

detect_int64(_) ->
    EncBin = <<18, 104, 0, 1, 0, 0, 0, 1, 0, 0, 0>>,
    int64 = ebson_decode:field_type(EncBin).    

%%%-------------------------------------------------------------------
%%% VALUES TESTS
%%%-------------------------------------------------------------------
decode_double(_) ->
    DblBin = <<174,71,225,122,20,174,243,63>>,
    {1.23, <<>>} = ebson_decode:value(float, DblBin).

decode_string(_) ->
    StrBin = <<3:1/integer-little-unit:32, 104, 104, 0>>,
    {<<"hh">>, <<>>} = ebson_decode:value(string, StrBin).

decode_binary(_) ->
    BinBin = <<4, 0, 0, 0, 0, 1, 2, 3, 4>>, %% 0 is intentional. it is the subtype
    {{binary, <<1, 2, 3, 4>>}, <<>>} = ebson_decode:value(binary, BinBin).

decode_bool_t(_) ->
    BoolTBin = <<1>>,
    {true, <<>>} = ebson_decode:value(bool, BoolTBin).

decode_bool_f(_) ->
    BoolFBin = <<0>>,
    {false, <<>>} = ebson_decode:value(bool, BoolFBin).

decode_utc(_) ->
    UTCBin = <<1, 2, 3, 4, 5, 6, 7, 8>>,
    {{unix_time, 578437695752307201}, <<>>} = ebson_decode:value(unix_time, UTCBin).

decode_null(_) ->
    NullBin = <<1, 2>>,
    {undefined, <<1, 2>>} = ebson_decode:value(null, NullBin).

decode_int32(_) ->
    Int32Bin = <<1, 2, 3, 4>>,
    {67305985, <<>>} = ebson_decode:value(int32, Int32Bin).	

decode_int64(_) ->
    Int64Bin = <<1, 2, 3, 4, 5, 6, 7, 8>>,
    {578437695752307201, <<>>} = ebson_decode:value(int64, Int64Bin).
     
decode_document(_) ->
    DocBin = <<5, 0, 0, 0, 0>>,
    {[], <<>>} = ebson_decode:value(document, DocBin).

decode_array(_) ->
    ArrBin = <<32,0,0,0,2,48,0,2,0,0,0,97,0,2,49,0,2,0,0,0,98,0,2,50,0,2,0,0,0,99,0,0>>,
    {[<<"a">>, <<"b">>, <<"c">>], <<>>} = ebson_decode:value(array, ArrBin).

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

complex_doc(_) -> % TODO refactor EncDoc here to some config or something
    EncDoc = <<83,0,0,0,1,97,0,174,71,225,122,20,174,243,63,2,98,0,3,0,0,0,98,98,0,3,99,0,
	       36,0,0,0,4,97,97,0,27,0,0,0,2,48,0,4,0,0,0,97,97,97,0,2,49,0,4,0,0,0,98,98,
	       98,0,0,0,16,100,0,50,0,0,0,18,101,0,194,136,114,252,0,0,0,0,0>>,
    [{<<"a">>, 1.23}, 
     {<<"b">>, <<"bb">>}, 
     {<<"c">>, [{<<"aa">>, [<<"aaa">>, <<"bbb">>]}]}, 
     {<<"d">>, 50}, 
     {<<"e">>, 4235364546}] = ebson_decode:document(EncDoc).

%%%-------------------------------------------------------------------
%%% UNGROUPED TESTS
%%%-------------------------------------------------------------------
valid_key(_Config) ->
    EncBin = <<104, 104, 104, 0>>,
    {<<"hhh">>, <<>>} = ebson_decode:key(EncBin).


