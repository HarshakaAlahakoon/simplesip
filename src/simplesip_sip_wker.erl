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

process_sip_req(SipRec, SocketRec) ->
	% ?info("LastState : ~p", [LastState]),
	case SipRec#sip_message.type of
		method ->
			case SipRec#sip_message.method of
				register ->
					case is_authorized(SocketRec, SipRec) of
						true ->
							send(trying(SipRec), SocketRec),
							timer:sleep(1000),
							send(registered(SipRec), SocketRec);
						false ->
							send(unauthorized(SipRec), SocketRec)
					end;
				invite ->
					% case is_authorized(SocketRec, SipRec) of
					% 	true ->
					% 		send(trying(SipRec), SocketRec),
					% 		timer:sleep(1000),
					% 		send(ringing(SipRec), SocketRec),
					% 		timer:sleep(3000),
					% 		send(ok_sdp(SipRec), SocketRec);
					% 	false ->
					% 		send(unauthorized(SipRec), SocketRec)
					% end;
					send(ringing(SipRec), SocketRec),
					timer:sleep(1000),
					send(ok_sdp(SipRec), SocketRec);
				bye ->
					send(ok(SipRec), SocketRec);
				cancel ->
					send(cancel(SipRec), SocketRec);
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
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

ringing(SipRec) ->
	?info("Ringing...", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 180 Ringing\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

ok(SipRec) ->
	?info("OK...", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 200 OK\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

ok_sdp(SipRec) ->
	?info("OK SDP...", []),
	Status = "SIP/2.0 200 OK\r\n",
	%% TODO:: branch ??
	%  ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str()
	Via = SipRec#sip_message.via ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ ";tag=" ++ Tag ++"\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	% SessExpir = "Session-Expires: 600;refresher=uac\r\n",
	% Date = "Date: " ++ httpd_util:rfc1123_date() ++ "\r\n",
	Contact = "Contact: " ++ SipRec#sip_message.to ++ "\r\n",
	ContentType = SipRec#sip_message.'content-type' ++ "\r\n",
	% UA = "User-Agent: Simplesip 1\r\n",
	Allow = "Allow: INVITE,ACK,BYE,CANCEL,OPTIONS,PRACK,REFER,NOTIFY,SUBSCRIBE,INFO,MESSAGE\r\n",
	% AllowEvents = "Allow-Events: presence, kpml, talk\r\n",
	Server = "Server: Simplesip/1.9.0\r\n",
	Supported = "Supported: replaces,norefersub\r\n",
	SdpRes = sdp_res(SipRec),
	ContentLen = lists:concat(["Content-Length: ", erlang:byte_size(binary:list_to_bin(SdpRes)), "\r\n"]),
	Status++Via++From++To++CallId++CSeq++ContentType++Contact++Allow++Server++Supported++ContentLen++"\r\n"++SdpRes.

session_progress(SipRec) ->
	?info("Session Progress...", []),
	Status = "SIP/2.0 183 Session Progress\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ ";tag=" ++ Tag ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	Contact = "Contact: " ++ SipRec#sip_message.to ++ "\r\n",
	Date = "Date: " ++ httpd_util:rfc1123_date() ++ "\r\n",
	ContentType = SipRec#sip_message.'content-type' ++ "\r\n",
	SdpRes = sdp_res(SipRec#sip_message.sdp_message),
	% ?info("SdpRes : ~p", [SdpRes]),
	ContentLen = lists:concat(["Content-Length: ", erlang:byte_size(binary:list_to_bin(SdpRes)), "\r\n"]),
	Status++Via++From++To++CallId++CSeq++Date++ContentType++ContentLen++"\r\n"++SdpRes.

registered(SipRec) ->
	?info("Registered...", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 200 OK\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	%% TODO:: sip or sips ??
	Contact = SipRec#sip_message.contact ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq++Contact++ContentLen++"\r\n".

unauthorized(SipRec) ->
	%% TODO:: sip version ??
	Status = "SIP/2.0 401 Unauthorized\r\n",
	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
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

is_authorized(SocketRec, SipRec) ->
	%% TODO:: edit this
	case ets:lookup(sip_connections, SocketRec) of
		[] ->
			SipConnRec = #sip_connection{
				socket_rec = SocketRec,
				authorized = true
			},
			ets:insert(sip_connections, SipConnRec),
			false;
		[SipConn] ->
			true
	end.

authorize(SipRec) ->
	%% TODO:: check authorization

	passed.

cancel(SipRec) ->
	%% TODO:: process current state before sending OK
	ok(SipRec).

sdp_res(SipRec) ->
	SdpMsg = SipRec#sip_message.sdp_message,
	V = "v=0\r\n",	% v,  	%
	[_, Called, _, _] = string:tokens(SipRec#sip_message.to, ":@"),
	SessId = (SdpMsg#sdp_message.o)#sdp_origin.session_id + 1,
	O = lists:concat(["o=", simplesip, " ", SessId, " ", SessId+1, " IN IP4 ", simplesip_sip_util:local_ip_v4_str(), "\r\n"]),		% o,  	%
	S = "s=-\r\n",		% s,  	%
	% i = []
	% u,  	%
	% e,  	%
	% p,  	%
	C = "c=IN IP4 " ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",		% c = []
	% b = []
	% z,  	%
	% k = []
	T = "t=0 0\r\n",	% t = []
	% r = []
	{MediaList, AttribList} = simplesip_sip_util:get_compatible_audio(SdpMsg#sdp_message.m, SdpMsg#sdp_message.a),
	M = concat_media(MediaList),
	Fmtp = "a=fmtp:101 0-15\r\n",
	A = concat_attributes(AttribList),	%% ++ "a=sendrecv\r\n",
	V++O++S++C++T++M++A++Fmtp.

concat_media(MediaList) ->
	Fun1 = fun(MediaRec, Acc) ->
		La = MediaRec#media.fmt_list,
		Spaces = lists:duplicate(length(MediaRec#media.fmt_list), " "),
		L = lists:zipwith(fun(X,Y)-> lists:concat([X, Y]) end, Spaces, MediaRec#media.fmt_list),
		FmtList = lists:concat(L),
		lists:concat([Acc, "m=audio ", MediaRec#media.port, " RTP/AVP", FmtList, "\r\n"])
	end,
	lists:foldl(Fun1, "", MediaList).

concat_attributes(AttribList) ->
	concat_attributes("", AttribList).
concat_attributes(Line, []) ->
	Line;
concat_attributes(Line, [Attrib | Rest]) ->
	case Attrib#attribute.type of
		property ->
			Str = "a=" ++ Attrib#attribute.attribute ++ "\r\n",
			concat_attributes(Line++Str, Rest);
		value ->
			case Attrib#attribute.value of
				#rtpmap{fmt = Fmt, encoding = Enc, clock_rate = Clk, encoding_para = []} ->
					Str = lists:concat(["a=rtpmap:", Fmt, " ", Enc, "/", Clk, "\r\n"]),
					concat_attributes(Line++Str, Rest);
				#rtpmap{fmt = Fmt, encoding = Enc, clock_rate = Clk, encoding_para = Para} ->
					Str = lists:concat(["a=rtpmap:", Fmt, " ", Enc, "/", Clk, "/", Para, "\r\n"]),
					concat_attributes(Line++Str, Rest);
				_ ->
					concat_attributes(Line, Rest)
			end
	end.

send(Data, SocketRec) ->
	gen_udp:send(SocketRec#socket_rec.socket, (SocketRec#socket_rec.client_addr)#client_addr.ip, (SocketRec#socket_rec.client_addr)#client_addr.in_port_no, Data).
