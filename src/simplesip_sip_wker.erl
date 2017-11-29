-module(simplesip_sip_wker).
% -behaviour(gen_server).
-compile(export_all).

-include("simplesip.hrl").

% -record(state, {udp_wker, prev_req}).
%
% start_link(Args) ->
% 	gen_server:start_link(simplesip_sip_wker, Args, []).
%
% init(Args) ->
% 	%% TODO:: set a timeout check whether connection is live and to close the socket
% 	{FromPid, SipRec} = Args,
% 	io:fwrite("~n~p:: Starting simplesip_sip_wker ~n, ~p~n", [?MODULE, SipRec]),
% 	State = #state{udp_wker = FromPid},
% 	process_sip_req(SipRec, State),
% 	{ok, State}.
%
% handle_cast(SipRec, State) when erlang:is_record(SipRec, sip_message)->
% 	process_sip_req(State#state.udp_wker, SipRec),
% 	{noreply, State};
% handle_cast(Request, State) ->
% 	{noreply, State}.
%
% handle_call(Request, From, State) ->
% 	{noreply, State}.
%
% % handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
% % 	io:fwrite("~n~p:: received from ~p: ~p~n", [?MODULE, Socket, Packet]),
% %     {noreply, State};
% % handle_info({udp_closed, Socket}, State) ->
% % 	io:fwrite("~n~p:: socket_closed ~p~n", [?MODULE, Socket]),
% %     {stop, {shutdown, socket_closed}, State};
% handle_info(Data, State) ->
% 	io:fwrite("~n~p:: received : ~p~n", [?MODULE, Data]),
%     {noreply, State}.
%
% code_change(OldVsn, State, Extra) ->
%     {ok, State}.
%
% terminate(Reason, State) ->
% 	io:fwrite("~n~p:: Terminting : ~p~n", [?MODULE, Reason]),
% 	ok.

%% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %%
%% --------------------------	SIP functions	----------------------------- %%

process_sip_req(SipRec, SocketRec, LastState) ->
	?info("LastState : ~p", [LastState]),
	case SipRec#sip_message.type of
		method ->
			case SipRec#sip_message.method of
				register ->
					if
						LastState ==  register ->
							case authorize() of
								passed ->
									send(trying(SipRec), SocketRec),
									timer:sleep(1000),
									send(registered(SipRec), SocketRec);
								denied ->
									error
							end;
						true ->
							send(unauthorized(SipRec), SocketRec)
					end;
				_ ->
					error
			end;
		status ->
			undefined
	end.

trying(SipRec) ->
	?info("Trying...", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 100 Trying\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

registered(SipRec) ->
	%% TODO:: sip version ??
	Status = "SIP/2.0 200 OK\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	%% TODO:: sip or sips ??
	% [ContactUri | _] = string:tokens(SipRec#sip_message.contact, ";"),
	% Contact = "Contact: " ++ ContactUri ++ ";expires=50" ++ "\r\n",
	Contact = SipRec#sip_message.contact ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq++Contact++ContentLen++"\r\n".

unauthorized(SipRec) ->
	%% TODO:: sip version ??
	Status = "SIP/2.0 401 Unauthorized\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	%% TODO:: sip or sips ??
	Authenticate = "WWW-Authenticate: Digest realm=harsha@192.168.8.100:8789, qop=\"auth\", nonce=\"\", opaque=\"\", stale=FALSE, algorithm=MD5\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq++Authenticate++ContentLen++"\r\n".

authorize() ->
	passed.

send(Data, SocketRec) ->
	gen_udp:send(SocketRec#socket_rec.socket, (SocketRec#socket_rec.client_addr)#client_addr.ip, (SocketRec#socket_rec.client_addr)#client_addr.in_port_no, Data).

local_ip_v4_str() ->
    {ok, Addrs} = inet:getifaddrs(),
    {A, B, C, D} = hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]),
	lists:concat([A, ".", B, ".", C, ".", D]).
