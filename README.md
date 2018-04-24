# Welcome to Simplesip!

## Introduction
Simplesip is a simple IVR server, and it is developed for educational purpose. But, this application can be extended further to use in industrial level. No performence/load tests have been done for this application and still in active development.


## How it works
The application starts the main supervisor `simplesip_sup`. Under this supervisor it starts:
 * `simplesip_tcp_wker_sup`,
 * `simplesip_rtp_streamer`
 * `simplesip_tcp_svr`
 * `simplesip_udp_svr`
 
 Here `simplesip_udp_svr` waits for a TCP connetion and accept it. Then it starts a `simplesip_tcp_wker` under
 `simplesip_tcp_wker_sup` (a `gwn_server`) to handle that connection further.
 
 Both TCP and UDP can be used for SIP protocol. But currently UDP is used for development.
 
 As UDP is a stateless protocol, no dedicated `gen_server`s are required to handle connections. Instead, `simplesip_udp_svr`
 receives UDP packet and spawn a worker process `simplesip_udp_wker` and hand-over the packet to that process.


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
