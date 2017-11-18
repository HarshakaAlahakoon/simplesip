-module(simplesip_app).
-behaviour(application).
-compile(export_all).

config_change(Changed, New, Removed) ->
	ok.

prep_stop(State) ->
	NewState = State,
	NewState.

start(StartType, StartArgs) ->
	{ok, Pid} = simplesip_sup:start_link(),
	{ok, Pid}.

start_phase(Phase, StartType, PhaseArgs) ->
	ok.

stop(State) ->
	ok.