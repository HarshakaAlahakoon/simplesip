# Welcome to Simplesip!

## Introduction
Simplesip is a simple IVR server, and it is developed for educational purpose. But, this application can be extended further to use in industrial level. No performence/load tests have been done for this application and still in active development.


## How to run
1. Compile : After cloning, open terminal in the project folder:

   `erlc -I include/ -o ebin/ src/*.erl`

2. Then, start an Erlang shell in ebin by issuing following commands in terminal:

   `cd ebin/`
   
   `erl -sname simplesip -setcookie test`
   
3. In Erlang shell, run:

   `application:start(sasl).`
  
   `application:load("simplesip.app").`
  
   `application:start(simplesip).`
