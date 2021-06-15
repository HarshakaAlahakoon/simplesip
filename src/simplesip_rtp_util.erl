-module(simplesip_rtp_util).
-compile(export_all).

-include("simplesip.hrl").

decode_rtp(Packet) ->
	% ?info("RTP Packet Length ::~p", [erlang:byte_size(Packet)]),
	<<H1:1/binary, H2:1/binary, SeqNum:2/binary, Rest1/binary>> = Packet,
	<<Version:2, Padding:1, Extention:1, CRSC_Count/bits>> = H1,
	<<Marker:1, PayLoadType/bits>> = H2,
	<<CRSC_CountInt:4>> = CRSC_Count,
	CRSC_length = 8 * CRSC_CountInt,
	<<TimeStamp:4/binary, SSRC:4/binary, CSRC_BinList:CRSC_length/binary, PayLoad/binary>> = Rest1,
	% <<PayLoadTypeInt:7>> = PayLoadType,
	TimeStampInt = binary:decode_unsigned(TimeStamp),
	SSRC_Int = binary:decode_unsigned(SSRC),
	if
		PayLoadType == ?TELEPHONE_EVENT ->
			?info("PayLoadType  :: ~p", [PayLoadType]);
		true ->
			ok
	end,
	#rtp_message{}.

% bit_to_int(_10_List) ->
% 	bit_to_int(0, 0, lists:reverse(_10_List)).
% bit_to_int(Int, _, []) ->
% 	Int;
% bit_to_int(Int, Power, [LeastBit | Rest]) ->
% 	NewInt = Int + erlang:trunc(math:pow(2, Power)),
% 	bit_to_int(NewInt, Power+1, Rest).

encode_rtp(PayLoad, SSRC_Int, SeqNumInt, IsMarker, Time) ->
	Version = <<2:2>>,		%% 2 bits long
	Padding = <<0:1>>,		%% 1 bit long
	Extention = <<0:1>>,
	CRSC_Count = <<0:4>>,
	H1 = <<Version/bits, Padding/bits, Extention/bits, CRSC_Count/bits>>,
	case IsMarker of
		true ->
			Marker = <<1:1>>;
		false ->
			Marker = <<0:1>>
	end,
	% PayLoadTypeInt = 0,	%% PCMU
	PayLoadTypeInt = 8,	%% PCMU
	PayLoadType = <<PayLoadTypeInt:7>>,
	H2 = <<Marker/bits, PayLoadType/bits>>,
	% Time = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	% TimeStamp = <<Time:32>>,
	TimeStamp = <<Time:32/big>>,
	%% TODO:: device a method for SSRC
	SSRC = <<SSRC_Int:32>>,
	%% TODO:: SeqNum should be incremented
	% SeqNum = <<SeqNumInt:16>>,
	SeqNum = <<SeqNumInt:16/big>>,
	Rest1 = <<TimeStamp/binary, SSRC/binary, PayLoad/binary>>,
	<<H1/binary, H2/binary, SeqNum/binary, Rest1/binary>>.

rtcp_receiver_report(SSRC_Int) ->
	Version = <<2:2>>,
	Padding = <<0:1>>,
	ReceptionReportCount = <<0:5>>,
	PacketType = <<201:8>>,
	%% length of this RTCP packet in 32-bit words minus one
	%% [(2 + 1 + 5 + 8 + 16 + 32) / 32] - 1
	Length = <<1:16>>,
	SSRC = <<SSRC_Int:32>>,
	H1 = <<Version/bits, Padding/bits, ReceptionReportCount/bits,PacketType/bits, Length/bits>>,
	<<H1/binary, SSRC/binary>>.

rtcp_source_description(SSRC_Int) ->
	Version = <<2:2>>,
	Padding_Bit = <<0:1>>,
	SourceCount = <<1:5>>,
	PacketType = <<202:8>>,		%% NOTE:: SDES : 202
	%---------------------------------------------------
	SSRC = <<SSRC_Int:32>>,
	C_Name = <<1:8>>,		%% NOTE:: CNAME (User and Domain) : 1
	Usr_and_Domain = erlang:list_to_binary("simplesip@" ++ simplesip_sip_util:local_ip_v4_str()),
	%% NOTE:: "byte" count of the name string
	LenInt = erlang:byte_size(Usr_and_Domain),
	Len = <<LenInt:8>>,
	N = (16 + LenInt) rem 32,
	if
		N == 0 ->
			Chunk_1 = <<C_Name/binary, Len/binary, Usr_and_Domain/binary>>;
		true ->
			EndPadding = binary:copy(<<0:8>>, (32-N) div 8),
			Chunk_1 = <<C_Name/binary, Len/binary, Usr_and_Domain/binary, EndPadding/binary>>
	end,
	%% length of this RTCP packet in 32-bit words minus one
	%% [(2 + 1 + 5 + 8 + 16 + 32) / 32] - 1
	ChunkLength = erlang:bit_size(Chunk_1) div 32,
	Length = <<(ChunkLength + 1):16>>,
	H1 = <<Version/bits, Padding_Bit/bits, SourceCount/bits,PacketType/bits, Length/bits>>,
	% ?info("H1 bits : ~p", [erlang:bit_size(H1)]),
	% ?info("SSRC bits : ~p", [erlang:bit_size(SSRC)]),
	% ?info("Usr_and_Domain bits : ~p", [erlang:bit_size(Usr_and_Domain)]),
	% ?info("Chunk bits : ~p", [erlang:bit_size(Chunk_1)]),
	<<H1/binary, SSRC/binary, Chunk_1/binary>>.

send_rtcp_start() ->
	SSRC_Int = 123456,
	D1 = rtcp_receiver_report(SSRC_Int),
	D2 = rtcp_source_description(SSRC_Int),
	RtcpData = <<D1/binary, D2/binary>>,
	[RtpConnRec | _] = ets:tab2list(rtp_connections),
	#media{port = EndPort} = RtpConnRec#rtp_connection.active_media,
	[#port_rec{protocol = rtcp, socket = RtcpSocket}] = ets:lookup(ports, rtcp),
	IP = RtpConnRec#rtp_connection.connection_address,
	send(RtcpSocket, IP, get_matching_rtcp_port(EndPort), RtcpData).

send_wav() ->
	?info("Reading audio file", []),
	[RtpConnRec | _] = ets:tab2list(rtp_connections),
	% {_, _, _, Data} = wave:read("/home/aryan/Desktop/tmp/test.wav"),
	% {ok, Data} = file:read_file("/home/aryan/Desktop/tmp/test.wav"),
	PrivDir = code:priv_dir(simplesip),
	{ok, PlaybackFile} = application:get_env(simplesip, playback_file),
	File = filename:join([PrivDir, "playback", PlaybackFile]),
	{ok, Data} = file:read_file(File),
	% ?info("Data : ~p", [Data]),
	[#port_rec{protocol = rtp, socket = RtpSocket}] = ets:lookup(ports, rtp),
	% send_wav(RtpConnRec, RtpSocket, 123456, 0, true, <<Data/binary>>).
	% Time = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	Time = 0,
	?info("Starting RTP stream", []),
	send_wav(RtpConnRec, RtpSocket, 123456, 0, true, Time, Data).
send_wav(_, _, _, _, _, _, <<>>) ->
	ok;
send_wav(RtpConnRec, RtpSocket, SSRC_Int, SeqNumInt, IsMarker, Time, Data) ->
	case erlang:byte_size(Data) of
		Size when Size >= 160 ->
			<<PayLoad:160/binary, Rest/binary>> = Data;
		Size ->
			PayLoad = <<Data:Size/binary>>,
			Rest = <<>>
	end,
	#media{port = EndPort} = RtpConnRec#rtp_connection.active_media,
	IP = RtpConnRec#rtp_connection.connection_address,
	timer:sleep(20),
	send(RtpSocket, IP, EndPort, encode_rtp(PayLoad, SSRC_Int, SeqNumInt, IsMarker, Time)),
	send_wav(RtpConnRec, RtpSocket, SSRC_Int, SeqNumInt+1, false, Time+160, Rest).

send(Data, SocketRec) ->
	gen_udp:send(SocketRec#socket_rec.socket, (SocketRec#socket_rec.client_addr)#client_addr.ip, (SocketRec#socket_rec.client_addr)#client_addr.in_port_no, Data).

send(Socket, IP, EndPort, Data) ->
	gen_udp:send(Socket, IP, EndPort, Data).

get_matching_rtcp_port(RtpPort) ->
	case RtpPort rem 2 of
		0 ->
			RtpPort + 1;
		1 ->
			RtpPort + 2
	end.
