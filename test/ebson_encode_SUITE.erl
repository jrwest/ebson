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
	 three_key_doc/1,
	 complex_doc/1,

	 %% DETECTS GROUP TESTS
	 detect_float/1,
	 detect_string_binary/1,
	 detect_string_list/1,
	 detect_document/1,
	 detect_implicit_array/1,
	 detect_explicit_array/1,
	 detect_binary/1,
	 detect_bool_t/1,
	 detect_bool_f/1,
	 detect_unix_time/1,
	 detect_null/1,
	 detect_int32/1,
	 detect_int64/1,

	 %% VALUES GROUP TESTS
	 float_val/1,
	 string_binary_val/1,
	 string_list_val/1,
	 binary_val/1,
	 bool_t_val/1,
	 bool_f_val/1,
	 unix_time_val/1,
	 null_val/1,
	 int32_val/1,
	 int64_val/1,
	 document_val/1,
	 implicit_array_val/1,
	 explicit_array_val/1,

	 %% KEYS GROUP TESTS
	 binary_key/1,
	 list_key/1,
	 integer_key/1,
	 atom_key/1
	]).

%%%-------------------------------------------------------------------
%%% COMMON TEST FUNCTIONS
%%%-------------------------------------------------------------------
groups() ->
    [{keys, 
      [shuffle],
      [binary_key, list_key, integer_key, atom_key]},
     {values,
      [shuffle],
      [float_val, string_binary_val, string_list_val,
       binary_val, bool_t_val, bool_f_val, unix_time_val,
       null_val, int32_val, int64_val,
       document_val, implicit_array_val, explicit_array_val]},
     {detects, 
      [shuffle],
      [detect_float, detect_string_binary, detect_string_list,
       detect_document, detect_implicit_array, detect_explicit_array,
       detect_binary, detect_bool_t, detect_bool_f,
       detect_unix_time, detect_null, detect_int32,
       detect_int64]},
     {documents,
      [shuffle],
      [empty_doc, one_key_doc, three_key_doc, complex_doc]}].

all() ->
    [{group, keys}, {group, detects}, {group, values}, {group, documents}].    


%%%-------------------------------------------------------------------
%%% KEYS TESTS
%%%-------------------------------------------------------------------
binary_key(_) ->
    Key = <<"a">>,
    <<$a, 0>> = ebson_encode:key(Key).
    
list_key(_) ->
    Key = "1",
    <<$1, 0>> = ebson_encode:key(Key).

integer_key(_) ->
    Key = 1,
    <<$1, 0>> = ebson_encode:key(Key).

atom_key(_) ->
    Key = abc,
    <<$a, $b, $c, 0>> = ebson_encode:key(Key).

%%%-------------------------------------------------------------------
%%% VALUES TESTS
%%%-------------------------------------------------------------------
float_val(_) ->
    Val = 1.23,
    <<174,71,225,122,20,174,243,63>> = ebson_encode:value(1, Val).

string_binary_val(_) ->
    Val = <<"abc">>,
    <<4, 0, 0, 0, $a, $b, $c, 0>> = ebson_encode:value(2, Val).

string_list_val(_) ->
    Val = "abc",
    <<4, 0, 0, 0, $a, $b, $c, 0>> = ebson_encode:value(2, Val).

binary_val(_) ->
    Val = {binary, <<1, 2, 3, 4>>},
    <<4, 0, 0, 0, 0, 1, 2, 3, 4>> = ebson_encode:value(5, Val).

bool_t_val(_) ->
    <<1:1/little-integer-unit:8>> = ebson_encode:value(8, true).

bool_f_val(_) ->
    Val = false,
    <<0:1/little-integer-unit:8>> = ebson_encode:value(8, false).

unix_time_val(_) ->
    Val = {unix_time, 578437695752307201},
    <<1, 2, 3, 4, 5, 6, 7, 8>> = ebson_encode:value(9, Val).

null_val(_) ->
    Val = undefined,
    <<>> = ebson_encode:value(10, Val).
    
int32_val(_) ->
    Val = 1,
    <<1, 0, 0, 0>> = ebson_encode:value(16, Val).

int64_val(_) ->
    Val = 578437695752307201,
    <<1, 2, 3, 4, 5, 6, 7, 8>> = ebson_encode:value(18, Val).

document_val(_) ->
    Val = [],
    <<5, 0, 0, 0, 0>> = ebson_encode:value(3, Val).

implicit_array_val(_) ->
    Val = [<<"a">>, <<"b">>],
    <<23,0,0,0,2,48,0,2,0,0,0,97,0,2,49,0,2,0,0,0,98,0,0>> = ebson_encode:value(4, Val).

explicit_array_val(_) ->
    Val = {array, [<<"a">>, <<"b">>]},
    <<23,0,0,0,2,48,0,2,0,0,0,97,0,2,49,0,2,0,0,0,98,0,0>> = ebson_encode:value(4, Val).

%%%-------------------------------------------------------------------
%%% DETECTS TESTS
%%%-------------------------------------------------------------------
detect_float(_) ->
    Val = 1.23,
    1 = ebson_encode:field_flag(Val).
	
detect_string_binary(_) ->
    Val = <<"abc">>,
    2 = ebson_encode:field_flag(Val).

detect_string_list(_) ->
    Val = "abc",
    2 = ebson_encode:field_flag(Val).

detect_document(_) ->
    Val = [{a, b}],
    3 = ebson_encode:field_flag(Val).

detect_implicit_array(_) ->
    Val = [<<"a">>],
    4 = ebson_encode:field_flag(Val).

detect_explicit_array(_) ->
    Val = {array, [1]},
    4 = ebson_encode:field_flag(Val).

detect_binary(_) ->
    Val = {binary, <<1, 2>>},
    5 = ebson_encode:field_flag(Val).

detect_bool_t(_) ->
    Val = true,
    8 = ebson_encode:field_flag(Val).

detect_bool_f(_) ->
    Val = false,
    8 = ebson_encode:field_flag(Val).

detect_unix_time(_) ->
    Val = {unix_time, 456534365475},
    9 = ebson_encode:field_flag(Val).

detect_null(_) ->
    Val = undefined,
    10 = ebson_encode:field_flag(Val).

detect_int32(_) ->
    Val = 1,
    16 = ebson_encode:field_flag(Val).

detect_int64(_) ->
    Val = 21435432543676,
    18 = ebson_encode:field_flag(Val).

%%%-------------------------------------------------------------------
%%% DOCUMENTS TESTS
%%%-------------------------------------------------------------------
empty_doc(_) ->
    Doc = [],
    <<5, 0, 0, 0, 0>> = ebson_encode:document(Doc).

one_key_doc(_) ->
    Doc = [{<<"a">>, 1.23}],
    Doc = ebson_decode:document(ebson_encode:document(Doc)).

three_key_doc(_) ->
    Doc = [{<<"a">>, 1.23}, {<<"b">>, <<"abc">>}, {<<"c">>, <<"abc">>}],
    Doc = ebson_decode:document(ebson_encode:document(Doc)).

complex_doc(_) ->
    Doc = [{<<"a">>, 1.23}, 
	   {<<"b">>, <<"bb">>}, 
	   {<<"c">>, [{<<"aa">>, [<<"aaa">>, <<"bbb">>]}]}, 
	   {<<"d">>, 50}, 
	   {<<"e">>, 4235364546}],
    Doc = ebson_decode:document(ebson_encode:document(Doc)).

