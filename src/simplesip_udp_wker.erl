-module(simplesip_udp_wker).
-behaviour(gen_server).
-compile(export_all).

-record(state, {socket}).

start_link(Args) ->
	gen_server:start_link(simplesip_udp_wker, Args, []).

init(Args) ->
	{socket, Socket} = Args,
	io:fwrite("~n~p:: Starting simplesip_udp_wker for socket ~p~n", [?MODULE, Socket]),
	{ok, #state{socket = Socket}}.

handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
	io:fwrite("~n~p:: received from ~p: ~p~n", [?MODULE, Socket, Packet]),
    {noreply, State};
handle_info({udp_closed, Socket}, State) ->
	io:fwrite("~n~p:: socket_closed ~p~n", [?MODULE, Socket]),
    {stop, {shutdown, socket_closed}, State};
handle_info(Data, State) ->
	io:fwrite("~n~p:: received : ~p~n", [?MODULE, Data]),
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
	io:fwrite("~n~p:: Terminting : ~p~n", [?MODULE, Reason]),
	ok.