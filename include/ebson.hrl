%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-type ebson_field_type() :: 'float' | 'string' | 'document' | 
			    'array' | 'binary' | 'bool' | 'unix_time' |
			    'null' | 'int32' | 'int64' | 'string'.
-type ebson_val() :: 'false' | 'true' | 'undefined' | ebson_pl_doc() | 
		     [any()] | number() | binary().
-type ebson_pl_doc() :: [{binary(), ebson_val()} | {_, _}].
