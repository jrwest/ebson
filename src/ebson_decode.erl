%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_decode).

-export([field_type/1, document/1, key/1, value/2]).

document(Bin) ->
    <<Size:32/integer-little-unit:1, Rest/binary>> = Bin,
    DocSize = Size-5,
    <<Doc:DocSize/binary, 0:8, _NewRest/binary>> = Rest,
    document(Doc, []).

document(<<>>, Acc) ->
    lists:reverse(Acc);
document(<<TypeFlag:1/binary-unit:8, Rest/binary>>, Acc) ->
    Type = field_type(TypeFlag),
    {Decoded, Tail} = next_key_val(Type, Rest),
    document(Tail, [Decoded | Acc]).

field_type(<<1, _/binary>>) ->
    float;
field_type(<<2, _/binary>>) ->
    string;
field_type(<<3, _/binary>>) ->
    document;
field_type(<<4, _/binary>>) ->
    array;
field_type(<<5, _/binary>>) ->
    binary;
field_type(Bin) when is_binary(Bin) ->
    throw(ebson_unknown_fieldtype).

key(Bin) ->
    key(Bin, <<>>).

key(<<0, Rest/binary>>, Acc) ->
    {Acc, Rest};
key(<<Byte:1/unit:8, Rest/binary>>, Acc) ->
    key(Rest, <<Acc/binary, Byte>>).


value(string, <<Size:1/integer-little-unit:32, Str/binary>>) ->
    StrSize = Size-1,
    <<Value:StrSize/binary, 0, Rest/binary>> = Str,
    {Value, Rest}.
		  
next_key_val(Type = string, Bin) when is_binary(Bin) ->
    {Key, Rest} = key(Bin),
    {Val, Rest1} = value(Type, Rest),
    {{Key, Val}, Rest1}.