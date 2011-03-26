%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_get).

-export([value/2, has_key/2, keys/1]).

-define(EBSON_EMPTY_BINARY, <<5, 0, 0, 0, 0>>).

value(_Key, ?EBSON_EMPTY_BINARY) ->
    undefined;
value(Key, Bin) ->
    Doc = strip_doc(Bin),
    get_value(Key, Doc).

has_key(_Key, ?EBSON_EMPTY_BINARY) ->
    false;
has_key(Key, Bin) ->
    Doc = strip_doc(Bin),
    do_has_key(Key, Doc).


keys(?EBSON_EMPTY_BINARY) ->
    [];
keys(Bin) ->
    Doc = strip_doc(Bin),
    get_keys(Doc, []).

get_value(_Key, <<>>) ->
    undefined;
get_value(Key, <<TypeFlag:1/binary-unit:8, AtKey/binary>>) ->
    FieldType = ebson_decode:field_type(TypeFlag),
    case ebson_decode:key(AtKey) of
	{Key, AtValue} ->
	    {Value, _} = ebson_decode:value(FieldType, AtValue),
	    Value;
	{_, AtValue} ->
	    AtNextFlag = skip_value(FieldType, AtValue),
	    get_value(Key, AtNextFlag)
    end.

do_has_key(_Key, <<>>) ->
    false;
do_has_key(Key, <<TypeFlag:1/binary-unit:8, AtKey/binary>>) ->
    FieldType = ebson_decode:field_type(TypeFlag),
    case ebson_decode:key(AtKey) of
	{Key, _} ->
	    true;
	{_, AtValue} ->
	    AtNextFlag = skip_value(FieldType, AtValue),
	    do_has_key(Key, AtNextFlag)
    end.

get_keys(<<>>, Acc) ->
    lists:reverse(Acc);    
get_keys(<<TypeFlag:1/binary-unit:8, AtKey/binary>>, Acc) ->
    FieldType = ebson_decode:field_type(TypeFlag),
    {Key, AtValue} = ebson_decode:key(AtKey),
    AtNextFlag = skip_value(FieldType, AtValue),
    get_keys(AtNextFlag, [Key | Acc]).

strip_doc(Bin) ->
    <<Size:4/little-signed-integer-unit:8, Rest/binary>> = Bin,
    DocSize = Size - 5,
    <<Doc:DocSize/binary-unit:8, 0>> = Rest,
    Doc.

%% THERE IS A MUCH BETTER WAY TO DO THIS
skip_value(float, <<_:8/little-float-unit:8, Rest/binary>>) ->
    Rest;    
skip_value(string, <<Size:1/integer-little-unit:32, Str/binary>>) ->
    StrSize = Size-1,
    <<_:StrSize/binary, 0, Rest/binary>> = Str,
    Rest;
skip_value(document, <<Size:1/integer-little-signed-unit:32, Rest/binary>>) ->
    DocSize = Size-4,
    <<_:DocSize/binary-unit:8, Tail/binary>> = Rest,
    Tail;
skip_value(array, <<Size:1/integer-little-signed-unit:32, Rest/binary>>) ->
    ArrSize = Size-4,
    <<_:ArrSize/binary-unit:8, Tail/binary>> = Rest,
    Tail;
skip_value(binary, <<Size:1/integer-little-unit:32, _SubType:1/integer-little-unit:8, Bin/binary>>) ->
    <<_:Size/binary, Rest/binary>> = Bin,
    Rest;
skip_value(bool, <<1, Rest/binary>>) ->
    Rest;
skip_value(bool, <<0, Rest/binary>>) ->
    Rest;
skip_value(unix_time, <<_:8/integer-little-signed-unit:8, Rest/binary>>) ->
    Rest;
skip_value(null, Rest) ->
    Rest;
skip_value(int32, <<_:4/integer-little-signed-unit:8, Rest/binary>>) ->
    Rest;
skip_value(int64, <<_:8/integer-little-signed-unit:8, Rest/binary>>) ->
    Rest.

    
