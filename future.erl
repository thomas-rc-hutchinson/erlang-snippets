%% Inspired by Scala Future. A naive approach at looking how they could be incorporated into Erlang.

-module(client).
-compile(export_all).

test() -> test(self()).
test(ClientPid) -> 
	spawn(fun() -> future([fun ws_lookup/0, fun db_lookup/0], ClientPid) end),
	receive
		Results -> handle_results(Results)
	end.

handle_results(Results) -> Results. %%do nothing



future([Task|Remaining], ClientPid) -> await_results(future(Remaining, self(), [spawn_future(Task, self())]), [], ClientPid).

%% spawn a process for each future
future([], Pid, FuturePids) -> FuturePids;
future([Task|Remaining], Pid, FuturePids) -> future(Remaining, Pid, FuturePids ++ [spawn_future(Task, Pid)]).


%% Wait for all futures to complete
await_results([], State, Pid) -> Pid ! State;
await_results(PidList, State, Pid) ->
	receive
		{From, Results} -> await_results(remove_pid(PidList, From, []), State ++ [Results], Pid)
	end.


remove_pid([Pid|Rest], Pid, NewPidList) -> NewPidList ++ Rest;
remove_pid([OtherPid|Rest], Pid, NewPidList) -> remove_pid(Rest, Pid, NewPidList ++ [OtherPid]).



%% Invoke fun and send results to Pid
spawn_future(Task, Pid) -> spawn(fun() -> Pid ! {self(), Task()} end).


%% mock a webservice lookup
ws_lookup() -> {ws, "web service response"}.
db_lookup() -> {db, "database response"}.
