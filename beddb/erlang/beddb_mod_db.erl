%%% File    : beddb_mod.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : Yaws Appmod for BedDB
%%%   http://en.wikipedia.org/wiki/REST#RESTful_example:_the_World_Wide_Web
%%% Created :  4 Feb 2008 by Renato Lucindo <lucindo@gmail.com>

-module(beddb_mod_db).
-author('lucindo@gmail.com').

-include("/opt/local/lib/yaws/include/yaws_api.hrl").

-export([out/1]).

out(A) ->
    Req = A#arg.req,
    handle(Req#http_request.method, A).

get_parans(A) ->
    Parans = string:tokens(A#arg.appmoddata, "/"),
    case length(Parans) of
	1 -> {table, lists:nth(1, Parans)};
	2 -> {table_and_id, lists:nth(1, Parans), lists:nth(2, Parans)};
	_ -> {error, Parans}
    end.

handle('GET', A) ->
    {table_and_id, TableStr, IdStr} = get_parans(A),
    Data = beddb:get(TableStr, IdStr),
    case yaws_api:queryvar(A, "format") of
	{ok, "json"} ->
	    {content, "application/json", beddb_str:record_to_json(Data)};
	undefined ->
	    {content, "text/plain", beddb_str:tuple_to_str(Data)}
    end;
handle('POST', A) ->
    {table, TableStr} = get_parans(A),
    Data = binary_to_list(A#arg.clidata),
    Response = beddb:put(TableStr, Data),
    {content, "text/plain", beddb_str:to_str(Response)};
handle('PUT', A) ->
    handle('POST', A);
handle('DELETE', A) ->
    {table_and_id, TableStr, IdStr} = get_parans(A),
    Response = beddb:del(TableStr, IdStr),
    {content, "text/plain", beddb_str:to_str(Response)}.
