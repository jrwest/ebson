%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_get_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([value_from_empty_binary_doc/1, 
	 value_from_first_key_in_binary_doc/1,
	 value_from_not_first_key_in_binary_doc/1,
	 has_key_is_false_for_any_key_when_empty_bin_doc/1,
	 has_key_is_true_for_first_key_in_bin_doc/1,
	 has_key_is_true_for_any_existing_key_in_bin_doc/1,
	 has_key_is_false_for_any_non_existing_key_in_bin_doc/1
	]).    

all() -> 
    [value_from_empty_binary_doc,
     value_from_first_key_in_binary_doc,
     value_from_not_first_key_in_binary_doc,
     has_key_is_false_for_any_key_when_empty_bin_doc,
     has_key_is_true_for_first_key_in_bin_doc,
     has_key_is_true_for_any_existing_key_in_bin_doc,
     has_key_is_false_for_any_non_existing_key_in_bin_doc].

value_from_empty_binary_doc(_) ->
    Doc = <<5, 0, 0, 0, 0>>,
    Key = <<"whocares">>,
    undefined = ebson_get:value(Key, Doc).

value_from_first_key_in_binary_doc(_) ->
    Key = <<"abc">>,
    Val = 1,
    Doc = ebson:encode([{Key, Val}]),
    Val = ebson_get:value(Key, Doc).

value_from_not_first_key_in_binary_doc(_) ->
    Key = <<"c">>,
    Val = 3,
    Doc = ebson:encode([{<<"a">>, 1}, {<<"b">>, 2}, {Key, Val}, {<<"d">>, 4}]),
    Val = ebson_get:value(Key, Doc).

has_key_is_false_for_any_key_when_empty_bin_doc(_) ->
    Key = <<"whocares">>,
    Doc = ebson:encode([]),
    false = ebson_get:has_key(Key, Doc).

has_key_is_true_for_first_key_in_bin_doc(_) ->
    Key = <<"abc">>,
    Doc = ebson:encode([{Key, 1}]),
    true = ebson_get:has_key(Key, Doc).

has_key_is_true_for_any_existing_key_in_bin_doc(_) ->
    Key = <<"c">>,
    Val = 3,
    Doc = ebson:encode([{<<"a">>, 1}, {<<"b">>, 2}, {Key, Val}, {<<"d">>, 4}]),
    true = ebson_get:has_key(Key, Doc).

has_key_is_false_for_any_non_existing_key_in_bin_doc(_) ->
    Key = <<"e">>,
    Doc = ebson:encode([{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3}, {<<"d">>, 4}]),
    false = ebson_get:has_key(Key, Doc).
    
