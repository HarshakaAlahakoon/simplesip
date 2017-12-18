-module(simplesip_sip_util).
-compile(export_all).

-include("simplesip.hrl").

decode_sip(Packet) ->
	% ?info("Packet : ~p", [Packet]),
	MsgStr = binary:bin_to_list(Packet),
  	StrList = string:tokens(MsgStr, "\r\n"),
	case extract_req_line(lists:nth(1, StrList)) of
    	error ->
      		%% TODO:: define a correct error format
			io:fwrite("~n~p::~p:: Error~n", [?MODULE, ?LINE]),
      		error;
    	SipReq ->
      		SipReqNew = extract_header(SipReq, lists:nthtail(1, StrList)),
			case SipReqNew#sip_message.'content-length' of
				Length when is_integer(Length) and (Length > 0) ->
					Start = erlang:byte_size(Packet) - Length,
					SdpBin = erlang:binary_part(Packet, {Start, Length}),
					%% TODO:: handle error and create #sip_message{}
					SdpMsgStr = binary:bin_to_list(SdpBin),
					SdpStrList = string:tokens(SdpMsgStr, "\r\n"),
					% io:fwrite("~n~p::~p:: Message body : ~p~n", [?MODULE, ?LINE, SdpStrList]),
					SdpRec = extract_sdp(#sdp_message{}, SdpStrList),
					% io:fwrite("~n~p~n~p~n", [SipReqNew, SdpRec]);
					% SdpRec;
					SipReqNew#sip_message{sdp_message = SdpRec};
				_ ->
					io:fwrite("~n~p::~p:: Empty message body~n", [?MODULE, ?LINE]),
					SipReqNew
			end
  	end.

%% determine the received message is whether SIP, and if it is SIP extract the request type or status code.
extract_req_line(Str) ->
	[Var1, Var2, Var3] = string:tokens(Str, " "),
  	Fields = case string:str(Var3, "SIP") of
    	0 ->
      		case string:str(Var1, "SIP") of
        		0 ->
          			error;
        		_ ->
          			{Code, _} = string:to_integer(Var2),
          			[{type, status}, {status, Code}]
      		end;
    	_ ->
      		Method = Var1,
      		MethodLow = string:to_lower(Method),
      		try erlang:list_to_existing_atom(MethodLow) of
        		MethodAtom ->
          			[{type, method}, {method, MethodAtom}]
      		catch
        		_:_ ->
          			MethodAtom = erlang:list_to_atom(MethodLow),
          			[{type, method},{method, MethodAtom}]
      		end
 	end,
	if
		Fields == error ->
			%% TODO:: define a correct error format
			error;
		true ->
			case Fields of
				[{type, status}, {status, StatsCode}] ->
					#sip_message{type = status, status = StatsCode};
				[{type, method},{method, MtdAtom}] ->
					#sip_message{type = method, method = MtdAtom, 'request-uri' = Var2}
		end
	end.

%% TODO:: add support for compact-types...ex: 'content-length'==l
extract_header(SipRec, []) ->
  	SipRec;
extract_header(SipRec, [Line | Rest]) ->
  	case string:chr(Line, $:) of
    	0 ->
      		extract_header(SipRec, Rest);
    	Position ->
      		Field = string:sub_string(Line, 1, Position-1),
      		% io:fwrite("~nField : ~p~n", [Field]),
      		FieldLow = string:to_lower(Field),
      		FieldlowAtom = try erlang:list_to_existing_atom(FieldLow) of
        		Atom ->
          			Atom
      		catch
        		_:_ ->
          			erlang:list_to_atom(FieldLow)
      		end,
      		NewSipRec = case FieldlowAtom of
          		accept ->
					%% TODO:: how to store and use this?
            		SipRec#sip_message{accept = Line};
          		'accept-encoding' ->
					%% TODO:: how to store and use this?
            		SipRec#sip_message{'accept-encoding' = Line};
          		to ->
            		SipRec#sip_message{to = string:sub_string(Line, Position+2)};
          		from ->
            		%% TODO:: do we need to extract dialog?
            		SipRec#sip_message{from = string:sub_string(Line, Position+2)};
          		'call-id' ->
            		% SipRec#sip_message{'call-id' = string:sub_string(Line, Position+2)};
            		SipRec#sip_message{'call-id' = Line};
          		via ->
            		SipRec#sip_message{via = lists:append(SipRec#sip_message.via, [Line])};
          		cseq ->
            		% [_, Seq, _] = string:tokens(Line, " "),
            		% {SeqInt, _} = string:to_integer(Seq),
            		% SipRec#sip_message{cseq = SeqInt};
					SipRec#sip_message{cseq = Line};
          		'max-forwards' ->
            		[_, MFw] = string:tokens(Line, " "),
            		{MFwInt, _} = string:to_integer(MFw),
            		SipRec#sip_message{'max-forwards' = MFwInt};
          		contact ->
            		% SipRec#sip_message{contact = string:sub_string(Line, Position+2)};
            		SipRec#sip_message{contact = Line};
          		'record-route' ->
            		SipRec#sip_message{'record-route' = string:sub_string(Line, Position+2)};
          		organization ->
            		SipRec#sip_message{organization = string:sub_string(Line, Position+2)};
          		'retry-after' ->
            		SipRec#sip_message{ 'retry-after' = string:sub_string(Line, Position+2)};
          		subject ->
            		SipRec#sip_message{subject = string:sub_string(Line, Position+2)};
          		supported ->
            		SipRec#sip_message{};
          		'session-expires' ->
            		%% TODO:: how to store and use this?
            		SipRec#sip_message{'session-expires' = string:sub_string(Line, Position+2)};
          		'content-type' ->
            		%% TODO:: how to store and use this?
            		SipRec#sip_message{'content-type' = Line};
          		'content-length' ->
					[_, Len] = string:tokens(Line, " "),
            		{LenInt, _} = string:to_integer(Len),
            		SipRec#sip_message{'content-length' = LenInt};
          		'user-agent' ->
					%% TODO:: how to store and use this?
            		SipRec#sip_message{'user-agent' = string:sub_string(Line, Position+2)};
          		_ ->
            		SipRec
    	end,
    	extract_header(NewSipRec, Rest)
	end.

extract_sdp(SdpRec, []) ->
	SdpRec;
extract_sdp(SdpRec, [Line | Rest]) ->
	%% NOTE:: match first charactor of the line
	Str = string:substr(Line, 3),
	% io:fwrite("~n~p::~p:: Line : ~p~n", [?MODULE, ?LINE, Str]),
	NewSdpRec = case string_to_atom(string:substr(Line, 1, 1)) of
		v ->
			SdpRec#sdp_message{v = Str};
		o ->
			[Uname, SessId, V, NetType, AddType, Addr] = string:tokens(Str, " "),
			SdpOri = #sdp_origin{
				username = Uname,
				session_id = simplesip_sip_util:string_to_int(SessId),
				version = simplesip_sip_util:string_to_int(V),
				network_type = NetType,
				address_type = AddType,
				address = Addr
			},
			SdpRec#sdp_message{o = SdpOri};
			% SdpRec#sdp_message{o = Line};
		s ->
			% SdpRec#sdp_message{s = Str};
			SdpRec#sdp_message{s = Line};
		i ->
			%% TODO:: how to store and use this?
			SdpRec#sdp_message{i = lists:append(SdpRec#sdp_message.i, [Str])};
		u ->
			%% TODO:: how to store and use this?
			SdpRec#sdp_message{u = Str};
		e ->
			SdpRec#sdp_message{e = Str};
		p ->
			SdpRec#sdp_message{p = Str};
		c ->
			TokenList = string:tokens(Str, " "),
			ConnData = #connection_data{
				network_type = lists:nth(1, TokenList),
				address_type = lists:nth(2, TokenList),
				connection_address = ip_str_to_erl_ip(lists:nth(3, TokenList))
			},
			SdpRec#sdp_message{c = ConnData};
		b ->
			%% TODO:: how to store and use this?
			SdpRec#sdp_message{b = lists:append(SdpRec#sdp_message.b, [Str])};
		z ->
			%% TODO:: how to store and use this?
			SdpRec#sdp_message{z = Str};
		k ->
			%% TODO:: how to store and use this?
			% EncKey = #encryption_key{
			% 	method,
			% 	key
			% },
			SdpRec#sdp_message{k = lists:append(SdpRec#sdp_message.k, [Str])};
		a ->
			SdpRec#sdp_message{a = lists:append(SdpRec#sdp_message.a, [extract_attribute(Str)])};
		t ->
			%% TODO:: how to store and use this?
			[StartStr, StopStr] = string:tokens(Str, " "),
			{Start, _} = string:to_integer(StartStr),
			{Stop, _} = string:to_integer(StopStr),
			TimeRec = #time{
				start = Start,
				stop = Stop
			},
			SdpRec#sdp_message{t = lists:append(SdpRec#sdp_message.t, [TimeRec])};
		r ->
			%% TODO:: how to store and use this?
			TokenList = string:tokens(Str, " "),
			RepeatRec = #repeat{
				repeat_interval = lists:nth(1, TokenList),
				active_duration = lists:nth(2, TokenList),
				offsets_from_start = lists:nthtail(2, TokenList)
			},
			SdpRec#sdp_message{r = lists:append(SdpRec#sdp_message.r, [RepeatRec])};
		m ->
			%% TODO:: how to store and use this?
			TokenList = string:tokens(Str, " "),
			Fun = fun(A) ->
				case string:to_integer(A) of
					{error, _} ->
						undefined;
					{Int, _} ->
						Int
				end
			end,
			Fmt = lists:map(Fun, lists:nthtail(3, TokenList)),
			MediaRec = #media{
				media = string_to_atom(string:to_lower(lists:nth(1, TokenList))),
				port = string_to_int(lists:nth(2, TokenList)),
				protocol = lists:nth(3, TokenList),
				fmt_list = Fmt
			},
			SdpRec#sdp_message{m = lists:append(SdpRec#sdp_message.m, [MediaRec])};
		_ ->
			SdpRec
	end,
	extract_sdp(NewSdpRec, Rest).

extract_attribute(Str) ->
	case string:tokens(Str, ":") of
		[Flag] ->
			#attribute{
				type = property,
				attribute = Flag
			};
		[Attr, Value] ->
			case string_to_atom(Attr) of
				NewAttr when NewAttr==rtpmap ->
					case string:tokens(Value, " /") of
						[Fmt, Enc, Clk] ->
							Para = [];
						[Fmt, Enc, Clk, Para] ->
							ok
					end,
					{FmtInt, _} = string:to_integer(Fmt),
					{ClkInt, _} = string:to_integer(Clk),
					NewValue = #rtpmap{
						fmt = FmtInt,
						encoding = Enc,
						clock_rate = ClkInt,
						encoding_para = Para
					},
					ok;
				NewAttr ->
					NewValue = Value
			end,
			#attribute{
				type = value,
				attribute = NewAttr,
				value = NewValue
			}
	end.

get_compatible_audio(MediaList, AttribList) ->
	%% TODO:: can this be passed from the udp_wker ??
	{ok, SvrRtpList} = application:get_env(simplesip, rtpmaps),
	% ?info("SvrRtpList : ~p", [SvrRtpList]),
	Fun1 = fun(Attrib, AccIn1) ->
		{AttribList1, FmtList} = AccIn1,
		if
			Attrib#attribute.attribute == rtpmap ->
				Rtp = Attrib#attribute.value,
				case lists:keyfind(Rtp#rtpmap.fmt, 2, SvrRtpList) of
					false ->
						AccIn1;
					SvrRtp ->
						if
							SvrRtp#rtpmap.clock_rate ==  Rtp#rtpmap.clock_rate ->
								case lists:member(Attrib, AttribList1) of
									true ->
										AccIn1;
									false ->
										?info("Attrib : ~p", [Attrib]),
										{AttribList1++[Attrib], FmtList++[Rtp#rtpmap.fmt]}
								end;
							true ->
								AccIn1
						end
				end;
			true ->
				AccIn1
		end
	end,
	Fun2 = fun(Media, AccIn2) ->
		% ?info("Media : ~p", [Media]),
		if
			%% TODO:: reconsider the protocol check
			(Media#media.media == audio) and (Media#media.protocol == "RTP/AVP")->
				{OldMediaList, OldAttrbList} = AccIn2,
				{NewAttrbList, FmtList}= lists:foldl(Fun1, {OldAttrbList, []}, AttribList),
				%% TODO:: what about a different port ??
				% {ok, Port} = application:get_env(simplesip, rtp_port),
				Port = 7000,
				{OldMediaList ++ [Media#media{port = Port, fmt_list = FmtList}], NewAttrbList};
			true ->
				AccIn2
		end
	end,
	lists:foldl(Fun2, {[], []}, MediaList).
%% ----------------------------------------------------------------------------------
string_to_atom(Str) ->
	try erlang:list_to_existing_atom(Str) of
		Atom ->
			Atom
	catch
		_:_ ->
			erlang:list_to_atom(Str)
	end.

string_to_int(Str) ->
	{Int, _} = string:to_integer(Str),
	Int.

ip_str_to_erl_ip(IpStr) ->
	[A, B, C, D] = string:tokens(IpStr, "."),
	AInt = string_to_int(A),
	BInt = string_to_int(B),
	CInt = string_to_int(C),
	DInt = string_to_int(D),
	{AInt, BInt, CInt, DInt}.

local_ip_v4_str() ->
    {ok, Addrs} = inet:getifaddrs(),
    {A, B, C, D} = hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]),
	lists:concat([A, ".", B, ".", C, ".", D]).

ip_to_str(Ip) ->
	{A, B, C, D} = Ip,
	lists:concat([A, ".", B, ".", C, ".", D]).
