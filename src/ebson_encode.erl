%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(ebson_encode).

-export([document/1, field_flag/1, key/1, value/2]).

document([]) ->
    <<5, 0, 0, 0, 0>>;
document(Doc) when is_list(Doc) ->
    document(Doc, <<>>).

document([], Bin) ->
    Size = byte_size(Bin)+5,
    <<Size:1/little-signed-integer-unit:32, Bin/binary, 0>>;
document([{Key, Val} | Doc], Bin) ->
    document(Doc, append_key_val(Key, Val, Bin)).

field_flag(Val) when is_float(Val) ->
    1;
field_flag(Val) when is_binary(Val)
		     orelse Val == []
		     orelse (is_list(Val) 
			     andalso length(Val) > 0 
			     andalso is_integer(hd(Val))) ->
    2;
field_flag([{_,_}|_]=_Val) ->
    3;
field_flag(Val) when is_list(Val) ->
    4;
field_flag({array, Val}) when is_list(Val) ->
    4;
field_flag({binary, Val}) when is_binary(Val) ->
    5;
field_flag(Val) when is_atom(Val) 
		     andalso (Val =:= true orelse Val =:= false) ->
    8;
field_flag({unix_time, Val}) when is_integer(Val) ->
    9;
field_flag(undefined) ->
    10;
field_flag(Val) when is_integer(Val) 
		     andalso Val >= -2147483648 
		     andalso Val =< 2147483647 ->
    16;
field_flag(Val) when is_integer(Val) ->
    18.

key(Key) when is_binary(Key) ->
    <<Key/binary, 0>>;
key(Key) when is_list(Key) ->
    EncKey = list_to_binary(Key),
    <<EncKey/binary, 0>>;
key(Key) when is_integer(Key) ->
    key(integer_to_list(Key));
key(Key) when is_atom(Key) ->
    key(atom_to_list(Key)).

value(1, Val) ->
    <<Val:8/float-little-unit:8>>;
value(2, Val) when is_binary(Val) ->
    StrSize = byte_size(Val) + 1,
    <<StrSize:1/little-signed-integer-unit:32, Val/binary, 0>>;
value(2, Val) when is_list(Val) ->
    value(2, list_to_binary(Val));
value(3, Val) ->
    document(Val);
value(4, {array, Val}) ->
    document(lists:zip(lists:seq(0, length(Val)-1), Val));
value(4, Val) ->
    document(lists:zip(lists:seq(0, length(Val)-1), Val));
value(5, {binary, Val}) when is_binary(Val) ->
    BinSize = byte_size(Val),
    <<BinSize:1/little-signed-integer-unit:32, 0, Val:BinSize/binary>>;
value(8, true) ->
    <<1:1/little-signed-integer-unit:8>>;
value(8, false) ->
    <<0:1/little-signed-integer-unit:8>>;
value(9, {unix_time, Val}) ->
    <<Val:8/little-signed-integer-unit:8>>;
value(10, undefined) ->
    <<>>;
value(16, Val) ->
    <<Val:4/little-signed-integer-unit:8>>;
value(18, Val) ->
    <<Val:8/little-signed-integer-unit:8>>.


append_key_val(Key, Val, Bin) ->
    CurrentSize = byte_size(Bin),
    FieldFlag = field_flag(Val),
    EncKey = key(Key),
    EncVal = value(FieldFlag, Val),
    <<Bin:CurrentSize/binary-unit:8, FieldFlag:1/integer-little-signed-unit:8, EncKey/binary, EncVal/binary>>.
