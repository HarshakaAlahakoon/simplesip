-module(simplesip_udp_svr).
-behaviour(gen_server).
-compile(export_all).

-include("simplesip.hrl").

-record(state, {
	listen_port,
	udp_conn_tab
}).

start_link() ->
	gen_server:start_link({local, simplesip_udp_svr}, simplesip_udp_svr, [], []).

init(Args) ->
	{ok, UdpPort} = application:get_env(simplesip, udp_port),
	{ok, UdpTab} = application:get_env(simplesip, udp_conn_tab),
	{ok, AcceptedSocket} = gen_udp:open(UdpPort, [binary, {active,true}, {reuseaddr, true}]),
	case ets:info(UdpTab) of
		undefined ->
			ets:new(UdpTab,[set, named_table, public, {keypos, 2}]);
		_ ->
			ok
	end,
	case ets:info(sip_connections) of
		undefined ->
			ets:new(sip_connections,[set, named_table, public, {keypos, 2}]);
		_ ->
			ok
	end,
	{ok, #state{listen_port = UdpPort, udp_conn_tab = UdpTab}}.

handle_cast(do_open, State) ->
	io:fwrite("~n~p:: Opening a UDP socket for port ~p~n", [?MODULE, State#state.listen_port]),
	{ok, AcceptedSocket} = gen_udp:open(State#state.listen_port, [binary, {active,true}, {reuseaddr, true}]),
	{noreply, State};
handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

handle_info({udp, Socket, IP, InPortNo, Packet} = Msg, State) ->
	ClientAddr = #client_addr{ip = IP, in_port_no = InPortNo},
	SocketRec =  #socket_rec{
		client_addr = ClientAddr,
		socket = Socket
	},
	proc_lib:spawn(simplesip_udp_wker, handle_udp_req, [Packet, SocketRec, State#state.udp_conn_tab]),
	{noreply, State};
handle_info({udp_closed, Socket}, State) ->
	io:fwrite("~n~p:: socket_closed ~p~n", [?MODULE, Socket]),
    {noreply, State};
handle_info(Info, State) ->
	{noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
	ok.
