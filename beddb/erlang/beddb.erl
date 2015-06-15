%%%-------------------------------------------------------------------
%%% File    : beddb.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : BedDB Mnesia wrapper
%%%   http://www.erlang.org/doc/apps/mnesia/index.html
%%%   http://trapexit.org/Distributing_a_Mnesia_schema
%%%   http://trapexit.org/Mnesia_Table_Fragmentation
%%%
%%% Created :  2 Feb 2008 by Renato Lucindo <lucindo@gmail.com>
%%%-------------------------------------------------------------------
-module(beddb).
-author('lucindo@gmail.com').

-include_lib("stdlib/include/qlc.hrl"). 

-import(beddb_str, [eval_string/1,
		    tuple_to_str/1]).
-export([start/0, 
	 stop/0, 
	 add_set/2,
	 add_set/4,
	 del_set/1,
	 add_index/2,
	 del_index/2,
	 add_node/2,
	 del_node/2,
	 add_frag/1,
	 del_frag/1,
	 select/1,
	 put/2,
	 get/2,
	 del/2]).

-define(MNESIA_DEFAULT_NODE_LIST, [node() | nodes()]).
-define(MNESIA_FRAGMENTS, 1).
-define(MNESIA_COPIES, 1).

start_schema() ->
    mnesia:stop(),
    mnesia:create_schema(?MNESIA_DEFAULT_NODE_LIST),
    mnesia:start().

start() ->
    start_schema().

stop() ->
    mnesia:stop().

add_set(TableNameStr, FieldListStr) ->
    add_set(TableNameStr, FieldListStr, "set", "[]").
add_set(TableNameStr, FieldListStr, TableTypeStr, IndexListStr) ->
    FragProp = {frag_properties,
 		[{node_pool, ?MNESIA_DEFAULT_NODE_LIST}, 
 		 {n_fragments, ?MNESIA_FRAGMENTS},
 		 {n_disc_only_copies, ?MNESIA_COPIES}]},
    mnesia:create_table(list_to_atom(TableNameStr), 
 			[FragProp,
 			 {type, list_to_atom(TableTypeStr)},
			 {index, eval_string(IndexListStr)},
 			 {disc_only_copies, ?MNESIA_DEFAULT_NODE_LIST}, 
			 {attributes, eval_string(FieldListStr)}]).

del_set(TableNameStr) ->
    mnesia:delete_table(list_to_atom(TableNameStr)).

add_index(TableNameStr, AttrNameStr) ->
    mnesia:add_table_index(list_to_atom(TableNameStr), list_to_atom(AttrNameStr)).

del_index(TableNameStr, AttrNameStr) ->
    mnesia:del_table_index(list_to_atom(TableNameStr), list_to_atom(AttrNameStr)).

add_node(TableNameStr, NodeNameStr) ->
    mnesia:change_table_frag(list_to_atom(TableNameStr), {add_node, list_to_atom(NodeNameStr)}).

del_node(TableNameStr, NodeNameStr) ->
    mnesia:change_table_frag(list_to_atom(TableNameStr), {del_node, list_to_atom(NodeNameStr)}).

add_frag(TableNameStr) ->
    Tab = list_to_atom(TableNameStr),
    mnesia:change_table_frag(Tab, {add_frag, mnesia:table_info(Tab, frag_dist)}).

del_frag(TableNameStr) ->
    mnesia:change_table_frag(list_to_atom(TableNameStr), del_frag).

put(TableNameStr, DataStr) ->
    AddFun = fun(Data) -> mnesia:write(list_to_atom(TableNameStr), Data, write) end,
    mnesia:activity(transaction, AddFun, [eval_string(DataStr)], mnesia_frag).

get(TableNameStr, IdStr) ->
    GetFun = fun(I) -> mnesia:read({list_to_atom(TableNameStr), I}) end,
    Elems = mnesia:activity(transaction, GetFun, [eval_string(IdStr)], mnesia_frag),
    lists:nth(1, Elems).

del(TableNameStr, IdStr) ->
    DelFun = fun(I) -> mnesia:delete({list_to_atom(TableNameStr), I}) end,
    mnesia:activity(transaction, DelFun, [eval_string(IdStr)], mnesia_frag).

select(QueryStr) ->
    QueryStrWithDot = beddb_str:ensure_ends_with_dot(QueryStr),
    QueryHandle = qlc:string_to_handle(QueryStrWithDot, [], erl_eval:new_bindings()),
    QueryFun = fun(Query) -> qlc:eval(Query) end,
    mnesia:activity(transaction, QueryFun, [QueryHandle], mnesia_frag).
