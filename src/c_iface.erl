-module(c_iface).
-compile(export_all).

start(RegName, ExtPrg) ->
    spawn(?MODULE, init, [RegName, ExtPrg]).
stop() ->
    c_iface ! stop.

% foo(X) ->
%     call_port({foo, X}).
% bar(Y) ->
%     call_port({bar, Y}).

% call_port(RegName, Msg) ->
%     RegName ! {call, self(), Msg},
%     receive
% 	{RegName, Result} ->
% 	    Result
%     end.

% loop(Port) ->
%     receive
% 	{call, Caller, Msg} ->
% 	    Port ! {self(), {command, encode(Msg)}},
% 	    receive
% 		{Port, {data, Data}} ->
% 		    Caller ! {c_iface, decode(Data)}
% 	    end,
% 	    loop(Port);
% 	stop ->
% 	    Port ! {self(), close},
% 	    receive
% 		{Port, closed} ->
% 		    exit(normal)
% 	    end;
% 	{'EXIT', Port, Reason} ->
% 	    exit(port_terminated)
%     end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

decode([Int]) -> Int.
