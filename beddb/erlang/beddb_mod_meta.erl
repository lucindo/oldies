%%% File    : beddb_mod.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : Yaws Appmod for BedDB (Metadata)
%%% Created :  6 Feb 2008 by Renato Lucindo <lucindo@gmail.com>

-module(beddb_mod_meta).
-author('lucindo@gmail.com').

-include("/opt/local/lib/yaws/include/yaws_api.hrl").
-import(lists, [nth/2]).
-export([out/1]).

out(A) ->
    Req = A#arg.req,
    Parans = string:tokens(A#arg.appmoddata, "/"),
    Result = process_command(Req#http_request.method, Parans, A),
    {content, "text/plain", beddb_str:to_str(Result)}.

process_command('PUT', Parans, A) ->
    case list_to_atom(nth(1, Parans)) of
	set -> 
	    Data = binary_to_list(A#arg.clidata),
	    beddb:add_set(nth(2, Parans), Data);
	index -> beddb:add_index(nth(2, Parans), nth(3, Parans));
	node -> beddb:add_node(nth(2, Parans), nth(3, Parans));
	fragment -> beddb:add_frag(nth(2, Parans));
	_ -> {error, "Unknown command"}
    end;
process_command('POST', Parans, A) ->
    process_command('PUT', Parans, A);
process_command('DELETE', Parans, _A) ->
    case list_to_atom(nth(1, Parans)) of
	set -> beddb:del_set(nth(2, Parans));
	index -> beddb:del_index(nth(2, Parans), nth(3, Parans));
	node -> beddb:del_node(nth(2, Parans), nth(3, Parans));
	fragment -> beddb:del_frag(nth(2, Parans));
	_ -> {error, "Unknown command"}
    end;
process_command('GET', _Parans, _A) ->
    {to_implement};
process_command(_, _, _) ->
    {error, "Unknown command"}.
