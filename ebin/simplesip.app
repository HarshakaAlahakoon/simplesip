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
		{udp_conn_tab, udp_connections}
	]}
 ]}.
