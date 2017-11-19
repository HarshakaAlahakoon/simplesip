-ifndef(SIMPLESIP_HRL).
-define(SIMPLESIP_HRL, 1).

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

-endif.
