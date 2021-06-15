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

process_sip_req(SipTab, OldConnRec, SipRec, SocketRec) ->
	% ?info("LastState : ~p", [LastState]),
	Now = calendar:local_time(),
	case SipRec#sip_message.type of
		method ->
			case SipRec#sip_message.method of
				register ->
					?info("Received messege: REGISTER", []),
					case is_authorized(SocketRec, SipRec) of
						true ->
							% send(trying(SipRec), SocketRec),
							% timer:sleep(1000),
							send(registered(SipRec, SocketRec), SocketRec),
							?info("Sent messege: REGISTERED", []);
						false ->
							send(unauthorized(SipRec), SocketRec),
							?info("Sent messege: UNAUTHORIZED", [])
					end;
				invite ->
					?info("Received messege: INVITE", []),
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

					if
						((OldConnRec /= []) and (OldConnRec#sip_connection.invite /= undefined)) ->
							if
								((OldConnRec#sip_connection.register)#sip_message.cseq == SipRec#sip_message.cseq)->
									% A Re-Request INVITE (i.e. A duplicate)
									ok;
								true ->
								% TODO:
									ok
									% NewSipConnRec = #sip_connection{
									% 	socket_rec = SocketRec,
									% 	invite = SipRec,
									% 	start_time = OldConnRec#sip_connection.start_time,
									% 	last_update = Now
									% },
									% ets:insert(SipTab, NewSipConnRec),
									% send(ringing(SipRec), SocketRec),
									% OK = ok_sdp(SipRec, SocketRec),
									% simplesip_rtp_util:send_rtcp_start(),
									% timer:sleep(2000),
									% send(OK, SocketRec),
									% timer:sleep(1000),
									% gen_server:cast(simplesip_rtp_streamer, send_wav)
									% % simplesip_rtp_util:send_wav();
							end;
						((OldConnRec /= []) and (OldConnRec#sip_connection.invite == undefined)) ->
							NewSipConnRec = #sip_connection{
								socket_rec = SocketRec,
								invite = SipRec,
								start_time = OldConnRec#sip_connection.start_time,
								last_update = Now
							},
							ets:insert(SipTab, NewSipConnRec),
							send(ringing(SipRec), SocketRec),
							?info("Sent messege: RINGING", []),
							OK = ok_sdp(SipRec, SocketRec),
							simplesip_rtp_util:send_rtcp_start(),
							timer:sleep(2000),
							send(OK, SocketRec),
							?info("Sent messege: OK", []),
							timer:sleep(1000),
							% gen_server:cast(simplesip_rtp_streamer, send_wav);
							simplesip_rtp_util:send_wav();
						true ->
							NewSipConnRec = #sip_connection{
								socket_rec = SocketRec,
								invite = SipRec,
								start_time = Now,
								last_update = Now
							},
							ets:insert(SipTab, NewSipConnRec),
							send(ringing(SipRec), SocketRec),
							OK = ok_sdp(SipRec, SocketRec),
							simplesip_rtp_util:send_rtcp_start(),
							timer:sleep(2000),
							send(OK, SocketRec),
							timer:sleep(1000),
							% gen_server:cast(simplesip_rtp_streamer, send_wav)
							simplesip_rtp_util:send_wav()
					end;
				ack ->
					?info("Received messege: ACK", []),
					% Do the thing here
					ok;
				bye ->
					?info("Received messege: BYE", []),
					send(ok(SipRec), SocketRec),
					?info("Sent messege: OK", []),
					NewSipConnRec = #sip_connection{
						socket_rec = SocketRec,
						start_time = OldConnRec#sip_connection.start_time,
						last_update = Now
					},
					ets:insert(SipTab, NewSipConnRec);
				cancel ->
					?info("Received messege: CANCEL", []),
					send(cancel(SipRec), SocketRec);
				_ ->
					error
			end;
		status ->
			undefined
	end.

trying(SipRec) ->
	?info("Composing messege: TRYING", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 100 Trying\r\n",
	% Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	Via = concat_via_values(SipRec#sip_message.via) ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	% Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from++ "\r\n",
	% To = "To: " ++ SipRec#sip_message.to ++ ";tag=" ++ Tag  ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

ringing(SipRec) ->
	?info("Composing message: RINGING ", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 180 Ringing\r\n",
	Via = concat_via_values(SipRec#sip_message.via) ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ ";tag=" ++ Tag ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq	++ContentLen++"\r\n".

ok(SipRec) ->
	?info("Composing message: OK", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 200 OK\r\n",
	Via = concat_via_values(SipRec#sip_message.via) ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to  ++ ";tag=" ++ Tag ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq++ContentLen++"\r\n".

ok_sdp(SipRec, SocketRec) ->
	?info("Composing messege: OK with SDP", []),
	Status = "SIP/2.0 200 OK\r\n",
	%% TODO:: branch ??
	%  ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str()
	Via = concat_via_values(SipRec#sip_message.via) ++ "\r\n",
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
	SdpRes = sdp_res(SipRec, SocketRec),
	ContentLen = lists:concat(["Content-Length: ", erlang:byte_size(binary:list_to_bin(SdpRes)), "\r\n"]),
	Status++Via++From++To++CallId++CSeq++ContentType++Contact++Allow++Server++Supported++ContentLen++"\r\n"++SdpRes.

% session_progress(SipRec) ->
% 	?info("Session Progress...", []),
% 	Status = "SIP/2.0 183 Session Progress\r\n",
% 	Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
% 	%% TODO:: what is tag and how to create it?
% 	Tag = "37GkEhwl6",
% 	From = "From: " ++ SipRec#sip_message.from ++ "\r\n",
% 	To = "To: " ++ SipRec#sip_message.to ++ ";tag=" ++ Tag ++ "\r\n",
% 	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
% 	CSeq = SipRec#sip_message.cseq ++ "\r\n",
% 	Contact = "Contact: " ++ SipRec#sip_message.to ++ "\r\n",
% 	Date = "Date: " ++ httpd_util:rfc1123_date() ++ "\r\n",
% 	ContentType = SipRec#sip_message.'content-type' ++ "\r\n",
% 	SdpRes = sdp_res(SipRec#sip_message.sdp_message),
% 	% ?info("SdpRes : ~p", [SdpRes]),
% 	ContentLen = lists:concat(["Content-Length: ", erlang:byte_size(binary:list_to_bin(SdpRes)), "\r\n"]),
% 	Status++Via++From++To++CallId++CSeq++Date++ContentType++ContentLen++"\r\n"++SdpRes.

registered(SipRec, SocketRec) ->
	?info("Composing messege: OK for REGISTER", []),
	%% TODO:: sip version ??
	%% TODO:: sip or sips ??
	Status = "SIP/2.0 200 OK\r\n",
	% Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	ViaTokens = string:tokens(lists:nth(1, SipRec#sip_message.via), ";"),
	% ?info("ViaTokens : ~p", [ViaTokens]),
	case lists:member("rport", ViaTokens) of
		true ->
			?info("rport TRUE", []),
			ViaList1 = lists:delete("rport", ViaTokens),
			ViaList2 = lists:map(fun(A) -> A ++ ";" end, ViaList1),
			% lists:duplicate(length(ViaList1), ";")
			ReceivedIp = simplesip_sip_util:ip_to_str((SocketRec#socket_rec.client_addr)#client_addr.ip),
			ReceivedPort = (SocketRec#socket_rec.client_addr)#client_addr.in_port_no,
			Via = lists:concat([ViaList2, "received=", ReceivedIp, ";rport=", ReceivedPort, "\r\n"]);
		false ->
			Via = concat_via_values(SipRec#sip_message.via) ++ "\r\n"
	end,
	%% TODO:: what is tag and how to create it?
	Tag = "37GkEhwl6",
	From = "From: " ++ SipRec#sip_message.from++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++ ";tag=" ++ Tag  ++ "\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	%% -------------------------------
	case string:str(SipRec#sip_message.contact, "+sip.instance=") of
		0 ->
			Contact = SipRec#sip_message.contact ++ "\r\n";
		Index ->
			% [_, ContactData] = string:tokens(SipRec#sip_message.contact, " "),
			% [SipUri | Rest] = string:tokens(ContactData, ";"),
			ContactRec = simplesip_sip_util:extract_contact_header(SipRec#sip_message.contact),
			% Gruu = create_gruu(SipUri, Rest),
			% Gruu = create_gruu(ContactURI, Rest),
			Gruu = simplesip_sip_util:create_gruu(ContactRec),
			Contact = "Contact: " ++ ContactRec#contact_header.contact_uri ++ ";" ++ Gruu ++ "expires=3600\r\n",
			?info("Contact : ~p", [Contact])
	end,
	%% -------------------------------
	Date = "Date: " ++ httpd_util:rfc1123_date() ++ "\r\n",
	ContentLen = "Content-Length: 0\r\n",
	Status++Via++From++To++CallId++CSeq++Contact++Date++ContentLen++"\r\n".

unauthorized(SipRec) ->
	?info("Composing messege: UNAUTHORIZED with challenge", []),
	%% TODO:: sip version ??
	Status = "SIP/2.0 401 Unauthorized\r\n",
	% Via = SipRec#sip_message.via ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	Via = concat_via_values(SipRec#sip_message.via) ++ ";received=" ++ simplesip_sip_util:local_ip_v4_str() ++ "\r\n",
	%% TODO:: what is tag and how to create it?
	Tag = "1410948204",
	From = "From: " ++ SipRec#sip_message.from ++ "\r\n",
	To = "To: " ++ SipRec#sip_message.to ++  ";tag=" ++ Tag ++"\r\n",
	CallId = SipRec#sip_message.'call-id' ++ "\r\n",
	CSeq = SipRec#sip_message.cseq ++ "\r\n",
	%% TODO:: sip or sips ??
	%% TODO:: Remove hardcoded values (ex: Digest realm)
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

sdp_res(SipRec, SocketRec) ->
	SdpMsg = SipRec#sip_message.sdp_message,
	V = "v=0\r\n",	% v,  	%
	% [_, Called, _, _] = string:tokens(SipRec#sip_message.to, ":@"),
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
	?info("Compatible media list: ~p~n", [MediaList]),
	?info("Compatible attrib list: ~p~n", [AttribList]),
	M = concat_media(MediaList),
	Fmtp = "a=fmtp:101 0-15\r\n",
	Ptime = 20,
	A = lists:concat([concat_attributes(AttribList), "a=ptime:", Ptime, "\r\n"]),	%% ++ "a=sendrecv\r\n",
	Now = calendar:local_time(),
	NewRtpConnRec = #rtp_connection{
		profile = #rtp_profile{
			caller = SipRec#sip_message.from,
			socket_rec = SocketRec
			},
		connection_address = (SdpMsg#sdp_message.c)#connection_data.connection_address,
		active_media = lists:keyfind(audio, 2, SdpMsg#sdp_message.m),
		media_types = MediaList,
		attributes = AttribList,
		ptime = Ptime,
		last_update = Now,
		start_time = Now
	},
	ets:insert(rtp_connections, NewRtpConnRec),
	V++O++S++C++T++M++A++Fmtp.

concat_media(MediaList) ->
	Fun1 = fun(MediaRec, Acc) ->
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

concat_via_values(Values) ->
  Count = length(Values),
  Fun = fun (Str, {AccStr, X}) ->
    if
	X > 1 ->
          {AccStr ++ Str ++ "\r\n", X - 1};
	true ->
	  {AccStr ++ Str, X - 1}
    end
  end,
  {ViaStr, _} = lists:foldl(Fun, {"", Count}, Values),
  ViaStr.
    


send(Data, SocketRec) ->
	gen_udp:send(SocketRec#socket_rec.socket, (SocketRec#socket_rec.client_addr)#client_addr.ip, (SocketRec#socket_rec.client_addr)#client_addr.in_port_no, Data).
