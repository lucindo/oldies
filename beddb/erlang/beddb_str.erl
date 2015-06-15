%%% File    : beddb_str.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : String utilities
%%% Created :  4 Feb 2008 by Renato Lucindo <lucindo@gmail.com>

-module(beddb_str).
-author('lucindo@gmail.com').

-export([eval_string/1,
	 to_str/1,
	 tuple_to_str/1,
	 tuple_list_to_str/1,
	 record_to_json/1,
	 ensure_ends_with_dot/1]).

ensure_ends_with_dot(String) ->
    LastDot = string:rchr(String, $.),
    LastChar = string:len(String) - 1,
    if 
	LastDot == 0 ->
	    String ++ ".";
	LastDot == LastChar ->
	    String;
	true ->
	    String ++ "."
    end.

eval_string(String) ->
    {ok, Tokens, _} = erl_scan:string(ensure_ends_with_dot(String)),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Data, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
    Data.

is_ascii(E) ->         
    if E < 0 -> false;  
       E > 255 -> false;
       true -> true      
    end.

is_string(E) ->
    case is_list(E) of
	false -> false;
	true -> lists:all(fun is_ascii/1, E)
    end.

term_to_str(T) ->
    case is_string(T) of 
	true -> lists:nth(1, io_lib:fwrite("~p", [T]));
	false -> 
	    if is_tuple(T) -> tuple_to_str(T);
	       true -> io_lib:write(T)
	    end
    end.

tuple_to_str(T, Pos) ->
    if Pos < size(T) ->
	    lists:append([term_to_str(element(Pos, T)), ",", tuple_to_str(T, Pos + 1)]);
       Pos == size(T) ->
	    term_to_str(element(Pos, T));
       true ->
	    ""
    end.

tuple_to_str(T) ->
    case is_tuple(T) of
	false -> "";
	true -> "{" ++ tuple_to_str(T, 1) ++ " }"
    end.

to_str(E) ->
    case is_atom(E) of
	true -> atom_to_list(E);
	false -> tuple_to_str(E)
    end.

tuple_list_to_str(TupleList, Pos) ->
    if Pos < length(TupleList) ->
	    lists:append([tuple_to_str(lists:nth(Pos, TupleList)), ",", tuple_list_to_str(TupleList, Pos + 1)]);
       Pos == length(TupleList) ->
	    tuple_to_str(lists:nth(Pos, TupleList));
       true ->
	    ""
    end.

tuple_list_to_str(TupleList) ->
    "[" ++ tuple_list_to_str(TupleList, 1) ++ "]".

build_prop_list(Record, Attributes, Pos, List) ->
    AttrLen = length(Attributes),
    if Pos < AttrLen ->
	    L = lists:append([{lists:nth(Pos, Attributes), element(Pos + 1, Record)}], List),
	    build_prop_list(Record, Attributes, Pos + 1, L);
       true ->
	    lists:reverse(List)
    end.

record_to_json(Record) ->
    Attributes = mnesia:table_info(element(1, Record), attributes),
    PropList = build_prop_list(Record, Attributes, 1, [{record, atom_to_list(element(1, Record))}]),
    json:encode({struct, PropList}).
