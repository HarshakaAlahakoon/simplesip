-module(simplesip_sip_util).
-compile(export_all).

-include("simplesip.hrl").

decode(Packet) ->
	MsgStr = binary:bin_to_list(Packet),
  	StrList = string:tokens(MsgStr, "\r\n"),
	case extract_req_line(lists:nth(1, StrList)) of
    	error ->
      		%% TODO:: define a correct error format
      		error;
    	SipReq ->
      		extract_header(SipReq, lists:delete(lists:nth(1, StrList), StrList))
  	end.

%% determine the received message is whether SIP, and if it is SIP extract the request type or status code.
extract_req_line(Str) ->
	% StrList = string:tokens(Str, " "),
	[Var1, Var2, Var3] = string:tokens(Str, " "),
 %  	Fields = case string:str(lists:last(StrList), "SIP") of
  	Fields = case string:str(Var3, "SIP") of
    	0 ->
      % 		case string:str(lists:nth(1, StrList), "SIP") of
      		case string:str(Var1, "SIP") of
        		0 ->
          			error;
        		_ ->
          	% 		{Code, _} = string:to_integer(lists:nth(2, StrList)),
          			{Code, _} = string:to_integer(Var2),
          			[{type, status}, {status, Code}]
      		end;
    	_ ->
      % 		[Method, ReqUri, _] = StrList,
      		Method = Var1,
      		MethodLow = string:to_lower(Method),
      		try erlang:list_to_existing_atom(MethodLow) of
        		MethodAtom ->
          			[{type, method},{method, MethodAtom}]
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

extract_header(SipRec, []) ->
  	SipRec;
extract_header(SipRec, StrList) ->
  	Line = lists:nth(1, StrList),
  	case string:chr(Line, $:) of
    	0 ->
      		extract_header(SipRec, lists:delete(Line, StrList));
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
            		SipRec#sip_message{'call-id' = string:sub_string(Line, Position+2)};
          		via ->
            		SipRec#sip_message{via = lists:append(SipRec#sip_message.via, [Line])};
          		cseq ->
            		[_, Seq, _] = string:tokens(Line, " "),
            		{SeqInt, _} = string:to_integer(Seq),
            		SipRec#sip_message{cseq = SeqInt};
          		'max-forwards' ->
            		[_, MFw] = string:tokens(Line, " "),
            		{MFwInt, _} = string:to_integer(MFw),
            		SipRec#sip_message{'max-forwards' = MFwInt};
          		contact ->
            		SipRec#sip_message{contact = string:sub_string(Line, Position+2)};
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
    	extract_header(NewSipRec, lists:delete(Line, StrList))
	end.
