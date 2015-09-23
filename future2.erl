%% Inspired by Scala Future. A naive approach at looking how they could be incorporated into Erlang.

-module(future2).
-compile(export_all).


test_future() -> 
	future(fun ws_lookup/0, self(), [fun increment/1, fun decrement/1, fun increment/1]),
	receive
		Result -> Result
	end.

test_future_seq() -> 
	future_seq(fun ws_future/0, self(), []),
	receive
		Result -> Result
	end.

ws_future() -> future(fun ws_lookup/0, []).

%% Sequence of Futures
future_seq(Functions, CallerPid, MapFunctions) -> 
	spawn(fun() -> CallerPid ! map(await_results(start_procsesses(Functions)), MapFunctions) end).

start_procsesses(Futures) -> start_procsesses(Futures, []).
start_procsesses([Function|Rest], FuturePids) -> start_procsesses(Rest, FuturePids ++ [Function()]).	

await_results(FuturePids) -> await_results(FuturePids, []).
await_results([], Resuls) -> Resuls;
await_results(FuturePids, Resuls) -> 
	receive
		{Pid, Result} -> await_results(remove_pid(FuturePids, Pid, []), Resuls ++ [Result])
	end.

remove_pid([Pid|Rest], Pid, NewPidList) -> NewPidList ++ Rest;
remove_pid([OtherPid|Rest], Pid, NewPidList) -> remove_pid(Rest, Pid, NewPidList ++ [OtherPid]).


%% Single Future
future(Function, MapFunctions) -> fun() -> future(Function, self(), MapFunctions) end.
future(Function, CallerPid, MapFunctions) -> 
	spawn(fun() -> CallerPid ! {self(), map(Function(), MapFunctions)} end).

map(State, []) -> State;
map(State, [MapFunction|Rest]) -> map(MapFunction(State), Rest).



%% App specific functions
ws_lookup() -> {ws, 1}.
increment({ws, Number}) -> {ws, Number + 1}.
decrement({ws, Number}) -> {ws, Number - 1}.
