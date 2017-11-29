-module(simplesip_udp_wker).
% -behaviour(gen_server).
-compile(export_all).

-include("simplesip.hrl").

% -record(state, {table_name, socket, sip_wker}).
%
% start_link(Args) ->
% 	gen_server:start_link(simplesip_udp_wker, Args, []).
%
% init(Args) ->
% 	%% TODO:: set a timeout check whethwr connection is live and to close the socket
% 	{Tab, SocketRec, Packet} = Args,
% 	io:fwrite("~n~p:: Starting simplesip_udp_wker for socket ~p~n", [?MODULE, SocketRec]),
% 	%% TODO:: handle error here
% 	{_, State} = handle_udp_req(Packet, #state{table_name = Tab, socket = SocketRec}),
% 	{ok, State}.
%
% handle_cast({response, Response}, State) ->
% 	%% TODO:: respond to the udp request
% 	io:fwrite("~n~p:: received ~p~n", [?MODULE, {response, Response}]),
% 	SocketRec = State#state.socket,
% 	Address = (SocketRec#socket_rec.client_addr)#client_addr.ip,
% 	Port = (SocketRec#socket_rec.client_addr)#client_addr.in_port_no,
% 	Socket = SocketRec#socket_rec.socket,
% 	gen_udp:send(Socket, Address, Port, Response),
% 	{noreply, State};
% handle_cast(Request, State) ->
% 	{noreply, State}.
%
% handle_call(Request, From, State) ->
% 	{noreply, State}.
%
% handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
% 	io:fwrite("~n~p:: received from ~p: ~p~n", [?MODULE, Socket, Packet]),
% 	{_, State} = handle_udp_req(Packet, State),
%     {noreply, State};
% handle_info({udp_closed, Socket}, State) ->
% 	io:fwrite("~n~p:: socket_closed ~p~n", [?MODULE, Socket]),
%     {stop, {shutdown, socket_closed}, State};
% handle_info(Data, State) ->
% 	io:fwrite("~n~p:: received : ~p~n", [?MODULE, Data]),
%     {noreply, State}.
%
% code_change(OldVsn, State, Extra) ->
%     {ok, State}.
%
% terminate(Reason, State) ->
% 	io:fwrite("~n~p:: Terminating : ~p~n", [?MODULE, Reason]),
% 	ets:delete_object(State#state.table_name, State#state.socket),
% 	ok.

%% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %%
%% --------------------------	UDP functions	----------------------------- %%

%% TODO:: recognize the packet/request type
handle_udp_req(Packet, SocketRec, UdpTab) ->
	Now = calendar:local_time(),
	case ets:lookup(UdpTab, SocketRec) of
		[] ->
			LastState = undefined,
			StartTime = Now,
			LastUpdate = Now;
		[UdpConnRec] ->
			LastState = UdpConnRec#udp_connection.last_state,
			StartTime = UdpConnRec#udp_connection.start_time,
			LastUpdate = Now
	end,
	UpdatedState = case simplesip_sip_util:decode_sip(Packet) of
		error ->
			LastState;
		SipRec ->
			simplesip_sip_wker:process_sip_req(SipRec, SocketRec, LastState),
			Address = (SocketRec#socket_rec.client_addr)#client_addr.ip,
			Port = (SocketRec#socket_rec.client_addr)#client_addr.in_port_no,
			Socket = SocketRec#socket_rec.socket,
			% gen_udp:send(Socket, Address, Port, Response),
			SipRec#sip_message.method
	end,
	NewUdpConnRec = #udp_connection{
		socket_rec = SocketRec,
		last_state = UpdatedState,
		start_time = StartTime,
		last_update = LastUpdate
	},
	ets:insert(UdpTab, NewUdpConnRec).
