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
-include("../include/ebson.hrl").

-spec document(binary()) -> ebson_pl_doc().
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

-spec field_type(binary()) -> ebson_field_type().			
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
field_type(<<8, _/binary>>) ->
    bool;
field_type(<<9, _/binary>>) ->
    unix_time;
field_type(<<10, _/binary>>) ->
    null;
field_type(<<16, _/binary>>) ->
    int32;
field_type(<<18, _/binary>>) ->
    int64;
field_type(Bin) when is_binary(Bin) ->
    throw(ebson_unknown_fieldtype).

-spec key(binary()) -> {binary(), binary()}.
key(Bin) ->
    key(Bin, <<>>).

key(<<0, Rest/binary>>, Acc) ->
    {Acc, Rest};
key(<<Byte:1/unit:8, Rest/binary>>, Acc) ->
    key(Rest, <<Acc/binary, Byte>>).

-spec value(ebson_field_type(), binary()) -> {term(), binary()}.
value(float, <<Value:8/little-float-unit:8, Rest/binary>>) ->
    {Value, Rest};    
value(string, <<Size:1/integer-little-unit:32, Str/binary>>) ->
    StrSize = Size-1,
    <<Value:StrSize/binary, 0, Rest/binary>> = Str,
    {Value, Rest};
value(document, <<Size:1/integer-little-signed-unit:32, Rest/binary>>) ->
    DocSize = Size-4,
    <<Doc:DocSize/binary-unit:8, Tail/binary>> = Rest,
    {document(<<Size:1/integer-little-signed-unit:32, Doc:DocSize/binary>>), Tail};
value(array, <<Size:1/integer-little-signed-unit:32, Rest/binary>>) ->
    ArrSize = Size-4,
    <<Doc:ArrSize/binary-unit:8, Tail/binary>> = Rest,
    DecDoc = document(<<Size:1/integer-little-signed-unit:32, Doc:ArrSize/binary>>),
    {[Val || {_IdxKey, Val} <- DecDoc], Tail};
value(binary, <<Size:1/integer-little-unit:32, _SubType:1/integer-little-unit:8, Bin/binary>>) ->
    <<Value:Size/binary, Rest/binary>> = Bin,
    {{binary, Value}, Rest};
value(bool, <<1, Rest/binary>>) ->
    {true, Rest};
value(bool, <<0, Rest/binary>>) ->
    {false, Rest};
value(unix_time, <<UnixTime:8/integer-little-signed-unit:8, Rest/binary>>) ->
    {{unix_time, UnixTime}, Rest};
value(null, Rest) ->
    {undefined, Rest};
value(int32, <<Int:4/integer-little-signed-unit:8, Rest/binary>>) ->
    {Int, Rest};
value(int64, <<Int:8/integer-little-signed-unit:8, Rest/binary>>) ->
    {Int, Rest}.
		  
next_key_val(Type, Bin) when is_binary(Bin) ->
    {Key, Rest} = key(Bin),
    {Val, Rest1} = value(Type, Rest),
    {{Key, Val}, Rest1}.
