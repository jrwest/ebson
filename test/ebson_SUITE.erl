%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_SUITE).

-include_lib("easy_test/include/easy_test.hrl").
-include_lib("proper/include/proper.hrl").

-export([doc/0]).
-export([prop_identity/0]).
-export([init_per_suite/1, end_per_suite/1]).


-define(EBSON_PROP_IDENTITY_RUNS, 5000).

init_per_suite(Config) ->
    {A,B,C} = now(),
    random:seed(A, B, C),
    Config.

end_per_suite(Config) ->
    ok.

test_identity(_) ->
    true = proper:quickcheck(prop_identity(), ?EBSON_PROP_IDENTITY_RUNS).

% THIS PROPERTY PURPOSEFULLY SKIPS THE EMPTY DOC CASE
% EMPTY DOC CASE CAN BE HANDLED SEPERATELY IN A UNIT TEST
% SEE doc/1 generator in this module.
prop_identity() ->
    ?FORALL(Doc, doc(), 
	    ?WHENFAIL(io:format("Failed On: ~p~n", [Doc]),
		      Doc == ebson:decode(ebson:encode(Doc)))).

doc() ->
    non_empty(list(key_val())).

key_val() ->
    tuple([ebson_key(), ebson_val()]).

ebson_key() ->
    non_empty(my_binary()).

ebson_val() ->
    ?LAZY(frequency([{30, float()},
		     {30, non_empty(my_binary())},
		     {1, doc()},
		     {2, non_empty(list(my_binary()))}
		    ])).

my_binary() ->
    ?LET(X, non_empty(list(integer(1, 254))), list_to_binary(X)).
