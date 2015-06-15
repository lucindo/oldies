%%% File    : beddb_mod.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : Yaws Appmod for BedDB (Admin)
%%% Created :  6 Feb 2008 by Renato Lucindo <lucindo@gmail.com>

-module(beddb_mod_admin).
-author('lucindo@gmail.com').

-include("/opt/local/lib/yaws/include/yaws_api.hrl").

-export([out/1]).

out(_A) ->
    {ehtml,
     [{p,[], "comming soon..."}]}.
