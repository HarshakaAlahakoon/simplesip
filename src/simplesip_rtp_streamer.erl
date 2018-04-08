-module(simplesip_rtp_streamer).
-behaviour(gen_server).
-compile(export_all).

-include("simplesip.hrl").

-record(state, {c_bin_dir, c_port}).

start_link() ->
	gen_server:start_link({local, simplesip_rtp_streamer}, simplesip_rtp_streamer, [], []).

init(Args) ->
	process_flag(trap_exit, true),
	{ok, Cbin} = application:get_env(simplesip, c_bin_dir),
	CProgBinFile = Cbin ++ "/wav_stream",
	Port = erlang:open_port({spawn, CProgBinFile}, [{packet, 2}]),
	{ok, #state{c_bin_dir = Cbin, c_port = Port}}.

handle_cast(send_wav, State) ->
	[RtpConnRec | _] = ets:tab2list(rtp_connections),
	#media{port = Port} = RtpConnRec#rtp_connection.active_media,
	{A, B, C, D} = RtpConnRec#rtp_connection.connection_address,
	% 192.168.1.100:8080 ==> A.B.C.D:E
	% 999 == 8bit
	% The port number is an unsigned 16-bit integer, so 65535
	% command : <<"sendwav">>
	% address : <<A:8, B:8, C:8, D:8, E:16>>
	Command = <<"sendwav">>,
	RtpEndPort = <<Port:16>>,
	Address = erlang:list_to_binary(simplesip_sip_util:ip_to_str(RtpConnRec#rtp_connection.connection_address)),
	Data = <<Command/binary, RtpEndPort/binary, Address/binary>>,
	State#state.c_port ! {self(), {command, Data}},
	{noreply, State};
handle_cast(test, State) ->
	State#state.c_port ! {self(), {command, c_iface:encode({foo, 1})}},
	{noreply, State};
handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	?info("received : ~p", [Msg]),
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
	ok.
