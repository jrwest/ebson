%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_get).

-export([value/2, has_key/2, keys/1, doc_size/1]).

-define(EBSON_EMPTY_BINARY, <<5, 0, 0, 0, 0>>).

value(_Key, ?EBSON_EMPTY_BINARY) ->
    undefined;
value(Key, Bin) when is_binary(Bin) ->
    Doc = strip_doc(Bin),
    get_encoded_value(Key, Doc);
value(Key, List) when is_list(List) ->
    get_decoded_value(Key, List).


has_key(_Key, ?EBSON_EMPTY_BINARY) ->
    false;
has_key(Key, Bin) when is_binary(Bin)->
    Doc = strip_doc(Bin),
    do_encoded_has_key(Key, Doc);
has_key(Key, List) when is_list(List) ->
    do_decoded_has_key(Key, List).


keys(?EBSON_EMPTY_BINARY) ->
    [];
keys(Bin) ->
    Doc = strip_doc(Bin),
    get_keys(Doc, []).

doc_size(Bin) when is_binary(Bin) ->
    <<Size:1/little-signed-integer-unit:32, _/binary>> = Bin,
    Size.

get_encoded_value(_Key, <<>>) ->
    undefined;
get_encoded_value(Key, <<TypeFlag:1/binary-unit:8, AtKey/binary>>) ->
    FieldType = ebson_decode:field_type(TypeFlag),
    case ebson_decode:key(AtKey) of
	{Key, AtValue} ->
	    {Value, _} = ebson_decode:value(FieldType, AtValue),
	    Value;
	{_, AtValue} ->
	    AtNextFlag = skip_value(FieldType, AtValue),
	    get_encoded_value(Key, AtNextFlag)
    end.

get_decoded_value(_K, []) ->
    undefined;
get_decoded_value(Key, [{Key, Val} | _]) ->
    Val;
get_decoded_value(Key, [ _ | Rest ]) ->
    get_decoded_value(Key, Rest).


do_encoded_has_key(_Key, <<>>) ->
    false;
do_encoded_has_key(Key, <<TypeFlag:1/binary-unit:8, AtKey/binary>>) ->
    FieldType = ebson_decode:field_type(TypeFlag),
    case ebson_decode:key(AtKey) of
	{Key, _} ->
	    true;
	{_, AtValue} ->
	    AtNextFlag = skip_value(FieldType, AtValue),
	    do_encoded_has_key(Key, AtNextFlag)
    end.

do_decoded_has_key(_Key, []) ->
    false;
do_decoded_has_key(Key, [{Key, _} | _]) ->
    true;
do_decoded_has_key(Key, [_ | Rest]) ->
    do_decoded_has_key(Key, Rest).


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

    
