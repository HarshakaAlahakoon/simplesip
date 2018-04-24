# Welcome to Simplesip!

## Introduction
Simplesip is a simple IVR server, and it is developed for educational purpose. But, this application can be extended further to use in industrial level. No performence/load tests have been done for this application and still in active development.

This application has been developed to use both TCP and UDP for SIP protocol. But currently UDP is used for development.
TCP support is developed to a certain extent.

Simplesip currently has the capability to detect DTMF tones and act accordingly. Call-flow features are under development in 
branch `call-flows-with-dtmf`.

## How it works
The application starts the main supervisor `simplesip_sup`. Under this supervisor it starts:
 * `simplesip_tcp_wker_sup`,
 * `simplesip_rtp_streamer`
 * `simplesip_tcp_svr`
 * `simplesip_udp_svr`
 
 Here `simplesip_udp_svr` waits for a TCP connetion and accept it. Then it starts a `simplesip_tcp_wker` under
 `simplesip_tcp_wker_sup` (a `gen_server`) to handle that connection further.
 
 As UDP is a stateless protocol, no dedicated `gen_server`s are required to handle connections. Instead, `simplesip_udp_svr`
 receives UDP packet and spawn a worker process `simplesip_udp_wker` and hand-over the packet to that process.
 
 According to the received port, UDP packets are identified whether they are SIP/RTP/RTCP. Then they are processed using 
 relevent model, i.e. `simplesip_sip_wker`.
 
 Most of the specialized functions are grouped together in a same module. For an example, functions for encoding/decoding SIP 
 packets are in `simplesip_sip_util`


## How to run
1. After cloning, open terminal in the project folder:

   `erlc -I include/ -o ebin/ src/*.erl`

2. Start an Erlang shell in ebin by issuing following commands in the same terminal:

   `cd ebin/`
   
   `erl -sname simplesip -setcookie test`
   
3. In Erlang shell, run:

   `application:start(sasl).`
  
   `application:load("simplesip.app").`
  
   `application:start(simplesip).`
