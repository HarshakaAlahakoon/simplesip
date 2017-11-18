-module(simplesip_tcp_svr).
-behaviour(gen_server).
-compile(export_all).

-record(state, {listen_port, listen_socket}).

start_link() ->
	gen_server:start_link({local, simplesip_tcp_svr}, simplesip_tcp_svr, [], []).

init(Args) ->
	{ok, ListenSocket} = gen_tcp:listen(8088, [{active,true}, {reuseaddr, true}]),
	gen_server:cast(self(), do_accept),
	{ok, #state{listen_port = 8088, listen_socket = ListenSocket}}.

handle_cast(do_accept, State) ->
	io:fwrite("~n~p:: Waiting for a connection request on ~p~n", [?MODULE, State#state.listen_socket]),
	{ok, AcceptedSocket} = gen_tcp:accept(State#state.listen_socket),
	io:fwrite("~n~p:: Connection request accepted: new tx socket ~p~n", [?MODULE, AcceptedSocket]),
	{ok, Pid} = supervisor:start_child(simplesip_tcp_wker_sup, [{socket, AcceptedSocket}]),
	gen_tcp:controlling_process(AcceptedSocket, Pid),
	gen_server:cast(self(), do_accept),
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