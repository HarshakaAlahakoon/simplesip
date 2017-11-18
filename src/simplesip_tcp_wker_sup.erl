-module(simplesip_tcp_wker_sup).
-behaviour(supervisor).
-compile(export_all).

start_link() ->
    supervisor:start_link({local, simplesip_tcp_wker_sup}, simplesip_tcp_wker_sup, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 1, 60},
        [{simplesip_tcp_wker, {simplesip_tcp_wker, start_link, []},
        	transient, brutal_kill, worker, [simplesip_tcp_wker]}
        ]}}.