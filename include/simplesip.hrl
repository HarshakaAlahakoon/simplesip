-ifndef(SIMPLESIP_HRL).
-define(SIMPLESIP_HRL, 1).

-define(info(Msg, Args), io:fwrite(
	"~n~p::~p:: "++Msg, [?MODULE, ?LINE]++Args)).

-record(client_addr, {
	ip,
	in_port_no
	}).

-record(socket_rec, {
	client_addr,
	socket
	}).

-record(udp_connection, {
	socket_rec,
	last_state,
	start_time,
	last_update
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%			SIP protocol		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(sip_message, {
  	type,
  	method,
  	status,
  	'request-uri',
  	accept,
  	'accept-encoding',
  	to,
  	from,
  	'call-id',
  	via = [],
  	cseq,
  	'max-forwards',
  	contact,
  	'record-route',
  	organization,
  	'retry-after',
  	subject,
  	supported,
  	'session-expires',
  	'content-type',
  	'content-length',
  	'user-agent',
  	sdp
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%			SDP protocol		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO:: redefine this record
-record(sdp_message, {
	v,  	% (protocol version)
	o,  	% (owner/creator and session identifier).
	s,  	% (session name)
	i = [],	% (session information)
	u,  	% (URI of description)
    e,  	% (email address)
    p,  	% (phone number)
    c = [], % (connection information - not required if included in all media)
    b = [], % (bandwidth information)
    z,  	% (time zone adjustments)
    k = [], % (encryption key)
    a = [],	% (zero or more media attribute lines), [{attribute, Value}]
	t = [],	% (time the session is active)
    r = [],	% (zero or more repeat times)
	m = []	% (media name and transport address)
	}).

-record(media, {
	media,				% media type
	port,				% transport port to which the media stream is sent
	protocol,			% transport protocol
	fmt_list = []		% media format descriptions
	}).

-record(rtpmap, {
	fmt,
	encoding,
	clock_rate,
	encoding_para = []
	}).

-record(sdp_origin, {
	username,
	session_id,
	version,
	network_type,
	address_type,
	address
	}).

-record(connection_data, {
	network_type,
	address_type,
	connection_address
	}).

-record(encryption_key, {
	method,
	key
	}).

-record(attribute, {
	type,	%% property (flags) / value (media types)
	flag,
	attribute,
	value
	}).

-record(time,{
	start,
	stop
	}).

-record(repeat, {
	repeat_interval,
	active_duration,
	offsets_from_start = []
	}).

-endif.
