# simplesip
A simple Erlang SIP/IVR server

Welcome to Simplesip! The Readme and Wiki is still not initiated.

==================================================================================================

1. Compile : After cloning, open terminal in the project folder:
   > erlc -I include/ -o ebin/ src/*.erl

2. Then, start an Erlang shell in ebin:
   > cd ebin/
   
   > erl -sname simplesip -setcookie test
   
3. In Erlang shell run:
  > application:start(sasl).
  
  > application:load("simplesip.app").
  
  > application:start(simplesip).
