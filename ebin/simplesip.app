{application, simplesip,
   	[{description, "simplesip application"},
   	{vsn, "0.1"},
   	{modules, [simplesip_app, simplesip_sup, simplesip_tcp_wker_sup, simplesip_tcp_svr]},
   	{registered, [simplesip_svr]},
   	{applications, [kernel, stdlib, sasl]},
   	{mod, {simplesip_app,[]}},
	{env, [
		{sip_port, 5000},
		{udp_port, 8789},
		{rtp_port, 8000},
		{rtpmaps, [{rtpmap, 3, "GSM", 8000, []}, {rtpmap, 101, "telephone-event", 8000, []}]},
		{udp_conn_tab, udp_connections}
	]}
 ]}.
