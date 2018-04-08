{application, simplesip,
   	[{description, "simplesip application"},
   	{vsn, "0.1"},
   	{modules, [simplesip_app, simplesip_sup, simplesip_tcp_wker_sup, simplesip_tcp_svr]},
   	{registered, [simplesip_svr]},
   	{applications, [kernel, stdlib, sasl]},
   	{mod, {simplesip_app,[]}},
	{env, [
		{sip_port, 8789},
		{rtp_port, 8081},
		% {rtpmaps, [{rtpmap, 3, "GSM", 8000, []}, {rtpmap, 101, "telephone-event", 8000, []}]},
		% {rtpmaps, [{rtpmap, 0, pcmu, 8000, []}, {rtpmap, 101, 'telephone-event', 8000, []}]},
		{rtpmaps, [{rtpmap, 8, pcma, 8000, []}, {rtpmap, 101, 'telephone-event', 8000, []}]},
		{sip_conn_tab, sip_connections},
		{rtp_conn_tab, rtp_connections},
		{c_bin_dir, "/media/aryan/DataDisk/workspace/c/wav_stream/Debug"}
	]}
 ]}.
