-module(simplesip_udp_svr).
-behaviour(gen_server).
-compile(export_all).

-include("simplesip.hrl").

-record(state, {
	sip_port,
	sip_socket,
	rtp_port,
	rtp_socket,
	rtcp_port,
	rtcp_socket,
	sip_conn_tab,
	rtp_conn_tab
}).

start_link() ->
	gen_server:start_link({local, simplesip_udp_svr}, simplesip_udp_svr, [], []).

init(Args) ->
	io:fwrite("simplesip_udp_svr starting...~n~n"),
	{ok, SipIp} = application:get_env(simplesip, sip_ip),
	{ok, SipPort} = application:get_env(simplesip, sip_port),
	{ok, RtpPort} = application:get_env(simplesip, rtp_port),
	{ok, SipTab} = application:get_env(simplesip, sip_conn_tab),
	{ok, RtpTab} = application:get_env(simplesip, rtp_conn_tab),
	{ok, SipSocket} = gen_udp:open(SipPort, [binary, {active,true}, {reuseaddr, true}, {ip, SipIp}]),
	{ok, RtpSocket} = gen_udp:open(RtpPort, [binary, {active,true}, {reuseaddr, false}, {ip, SipIp}]),
	RTCP_Port = simplesip_rtp_util:get_matching_rtcp_port(RtpPort),
	{ok, RTCP_Socket} = gen_udp:open(RTCP_Port, [binary, {active,true}, {reuseaddr, true}]),
	case ets:info(SipTab) of
		undefined ->
			ets:new(SipTab,[set, named_table, public, {keypos, 2}]);
		_ ->
			ok
	end,
	case ets:info(RtpTab) of
		undefined ->
			ets:new(RtpTab,[set, named_table, public, {keypos, 2}]);
		_ ->
			ok
	end,
	case ets:info(ports) of
		undefined ->
			ets:new(ports,[set, named_table, public, {keypos, 2}]);
		_ ->
			ok
	end,
	ets:insert(ports, [
		#port_rec{protocol = sip, port = SipPort, socket = SipSocket},
		#port_rec{protocol = rtp, port = RtpPort, socket = RtpSocket},
		#port_rec{protocol = rtcp, port = RTCP_Port, socket = RTCP_Socket}
	]),
	{ok, #state{
		sip_port = SipPort,
		sip_socket = SipSocket,
		rtp_port = RtpPort,
		rtp_socket = RtpSocket,
		rtcp_port = RTCP_Port,
		rtcp_socket = RTCP_Socket,
		sip_conn_tab = SipTab,
		rtp_conn_tab = RtpTab
		}
	}.

% handle_cast(do_open, State) ->
% 	io:fwrite("~n~p:: Opening a UDP socket for port ~p~n", [?MODULE, State#state.listen_port]),
% 	{ok, AcceptedSocket} = gen_udp:open(State#state.listen_port, [binary, {active,true}, {reuseaddr, true}]),
% 	{noreply, State};
handle_cast(show_state, State) ->
	?info("State : ~p", [State]),
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
	if
		Socket == State#state.sip_socket ->
			proc_lib:spawn(simplesip_udp_wker, handle_sip_req, [Packet, SocketRec, State#state.sip_conn_tab]);
		Socket == State#state.rtp_socket ->
			% ?info("RTP Packet", []),
			proc_lib:spawn(simplesip_udp_wker, handle_rtp_msg, [Packet, SocketRec]);
		Socket == State#state.rtcp_socket ->
			% TODO: Process RTCP Packet
			?info("RTCP Packet", []);
		true ->
			?info("Unknown Packet received : Socket : ~p", [Socket]),
			?info("Unknown Packet : ~p", [Packet]),
			ok
	end,

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
