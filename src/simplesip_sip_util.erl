-module(simplesip_sip_util).
-compile(export_all).

decode(Packet) ->
  MsgStr = binary:bin_to_list(Packet),
  StrList = string:tokens(MsgStr, "\r\n"),
  {_, Type} = msg_type(lists:nth(1, StrList)),
  io:fwrite("~n~p:: Type : ~p~n", [?MODULE, Type]).

msg_type(Str) ->
  StrList = string:tokens(Str, " "),
  case string:str(lists:last(StrList), "SIP") of
    0 ->
      case string:str(lists:nth(1, StrList), "SIP") of
        0 ->
          error;
        _ ->
          {Code, _} = string:to_integer(lists:nth(2, StrList)),
          {status, Code}
      end;
    _ ->
      RequestFunc = erlang:list_to_existing_atom(string:to_lower(lists:nth(1, StrList))),
      {request, RequestFunc}
  end.

test() ->
  decode(sip_reject_sample()).

sip_register_sample() ->
  <<"REGISTER sip:sip.cybercity.dk SIP/2.0\r\nVia: SIP/2.0/UDP 192.168.1.2;branch=z9hG4bKnp149505178-438c528b192.168.1.2;rport\r\n
  From: <sip:voi18063@sip.cybercity.dk>;tag=8e948b0\r\nTo: <sip:voi18063@sip.cybercity.dk>\r\n
  Call-ID: 578222729-4665d775@578222732-4665d772\r\n
  Contact:  <sip:voi18063@192.168.1.2:5060;line=9c7d2dbd8822013c>;expires=1200;q=0.500\r\n
  Expires: 1200\r\n
  CSeq: 69 REGISTER\r\n
  Content-Length: 0\r\n
  Authorization: Digest username=\"voi18063\",realm=\"sip.cybercity.dk\",uri=\"sip:192.168.1.2\",nonce=\"1701af566be182070084c6f740706bb\",opaque=\"1701a1351f70795\",nc=\"00000001\",response=\"bd79fecae600a2eb79d37ec73214830b\"\r\n
  Max-Forwards: 70\r\n
  User-Agent: Nero SIPPS IP Phone Version 2.0.51.16\r\n
  \r\n">>.

sip_reject_sample() ->
  <<"SIP/2.0 401 Unauthorized\r\n
  Call-ID: 578222729-4665d775@578222732-4665d772\r\n
  CSeq: 68 REGISTER\r\n
  From: <sip:voi18063@sip.cybercity.dk>;tag=903df0a\r\n
  To: <sip:voi18063@sip.cybercity.dk>;tag=00-04092-1701af62-120c67172\r\n
  Via: SIP/2.0/UDP 192.168.1.2;received=80.230.219.70;rport=5060;branch=z9hG4bKnp151248737-46ea715e192.168.1.2\r\n
  WWW-Authenticate: Digest realm=\"sip.cybercity.dk\",nonce=\"1701af566be182070084c6f740706bb\",opaque=\"1701a1351f70795\",stale=false,algorithm=MD5\r\n
  Content-Length: 0\r\n\r\n">>.
