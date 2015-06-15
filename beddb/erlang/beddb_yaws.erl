%%% File    : beddb_yaws.erl
%%% Author  : Renato Lucindo <lucindo@gmail.com>
%%% Description : Yaws start/stop/setup
%%% Created :  4 Feb 2008 by Renato Lucindo <lucindo@gmail.com>

-module(beddb_yaws).
-author('lucindo@gmail.com').

-export([start/0,
	 start/1,
	 start/2,
	 stop/0,
	 out/1]).

-define(DEFAULT_LISTEN, {0,0,0,0}).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_DOC_ROOT, "/dev/null").
-define(DEFAULT_LOG_DIR, "/tmp").
-define(DEFAULT_TMP_DIR, "/tmp").

start() ->
    start(?DEFAULT_LISTEN, ?DEFAULT_PORT).
start(Port) ->
    start(?DEFAULT_LISTEN, Port).
start(Ip, Port) ->
    ServerConf = [{servername, "beddb"},
		  {port, Port},
		  {listen, Ip},
		  {appmods, [{"/db", beddb_mod_db},
			     {"/beddb", beddb_mod_db},
			     {"/meta", beddb_mod_meta},
			     {"/admin", beddb_mod_admin},
			     {"/", beddb_yaws}] }],
    GlobalConf = [ { logdir, ?DEFAULT_LOG_DIR }, 
		   { tmpdir, ?DEFAULT_TMP_DIR },
		   { ebindir, ["."] } ],
    ok = beddb:start(),
    yaws:start_embedded(?DEFAULT_DOC_ROOT, ServerConf, GlobalConf).

stop() ->
    yaws:stop(),
    application:unload(yaws),
    beddb:stop().

out(_A) ->
    {ehtml,
     [{center,[], "BedDB pre-experimental"}]}.
