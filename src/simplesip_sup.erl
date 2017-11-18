-module(simplesip_sup).
-behaviour(supervisor).
-compile(export_all).

start_link() ->
    supervisor:start_link({local, simplesip_sup}, simplesip_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
        [{simplesip_tcp_wker_sup, {simplesip_tcp_wker_sup, start_link, []},
        	permanent, infinity, supervisor, [simplesip_socket_sup]},
        {simplesip_tcp_svr, {simplesip_tcp_svr, start_link, []},
        	permanent, brutal_kill, worker, [simplesip_tcp_svr]}
        ]}}.