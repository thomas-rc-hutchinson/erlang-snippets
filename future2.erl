%% Inspired by Scala Future. A naive approach at looking how they could be incorporated into Erlang.

-module(future2).
-compile(export_all).





test_future() -> 
	future(fun ws_lookup/0, self(), [fun increment/1, fun decrement/1, fun increment/1]),
	receive
		Result -> Result
	end.

test() -> 
	alert("test()", self()),
	future_seq([ws_future([fun increment/1]), ws_future([fun decrement/1])], self(), [fun increment/1]),
		receive
			Result -> Result
		after 5000 -> null
	end.


ws_future(MapFunctions) -> future(fun ws_lookup/0, MapFunctions).
ws_future() -> future(fun ws_lookup/0, []).

%% Sequence of Futures
future_seq(Functions, CallerPid, MapFunctions) -> 
	spawn(fun() -> CallerPid ! map(await_results(start_procsesses(Functions)), MapFunctions) end).

start_procsesses(Functions) -> start_procsesses(Functions, []).
start_procsesses([], FuturePids) -> FuturePids;
start_procsesses([Function|Rest], FuturePids) -> start_procsesses(Rest, FuturePids ++ [Function()]).	

await_results(FuturePids) -> await_results(FuturePids, []).
await_results([], Resuls) -> Resuls;
await_results(FuturePids, Resuls) -> 
	receive
		{Pid, Result} -> await_results(remove_pid(FuturePids, Pid, []), Resuls ++ [Result])
	after 1000 -> timeout
	end.

remove_pid([Pid|Rest], Pid, NewPidList) -> NewPidList ++ Rest;
remove_pid([OtherPid|Rest], Pid, NewPidList) -> remove_pid(Rest, Pid, NewPidList ++ [OtherPid]).


%% Single Future
future(Function, MapFunctions) -> fun() -> 
	alert("future(_,_)", self()), 
	future(Function, self(), MapFunctions) end.




future(Function, CallerPid, MapFunctions) -> 
	spawn(fun() -> alert("future(_,_,_).spawn", self()), CallerPid ! {self(), map(Function(), MapFunctions)} end).

map(State, []) -> State;
map(State, [MapFunction|Rest]) -> map(MapFunction(State), Rest).


alert(From, Pid) -> io:format("From:~p Pid:~p ~n", [From, Pid]).


%% App specific functions
ws_lookup() -> {ws, 1}.


increment([], State) -> State;
increment([{ws, Number}|Rest], State) -> increment(Rest, State ++ [{ws, Number + 1}]).

increment(List) when is_list(List) -> increment(List, []);
increment({ws, Number}) -> {ws, Number + 1}.
decrement({ws, Number}) -> {ws, Number - 1}.
