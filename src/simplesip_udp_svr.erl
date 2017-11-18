-module(simplesip_udp_svr).
-behaviour(gen_server).
-compile(export_all).

-record(state, {listen_port}).

start_link() ->
	gen_server:start_link({local, simplesip_udp_svr}, simplesip_udp_svr, [], []).

init(Args) ->
	{ok, #state{listen_port = 8789}}.

handle_cast(do_open, State) ->
	io:fwrite("~n~p:: Opening a UDP socket for port ~p~n", [?MODULE, State#state.listen_port]),
	{ok, AcceptedSocket} = gen_udp:open(8789, [binary, {active,true}, {reuseaddr, true}]),
	{ok, Pid} = supervisor:start_child(simplesip_udp_wker_sup, [{socket, AcceptedSocket}]),
	gen_tcp:controlling_process(AcceptedSocket, Pid),
	{noreply, State};
handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	io:fwrite("~n~p:: received : ~p~n", [?MODULE, Msg]),
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
	ok.