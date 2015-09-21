%% Inspired by Scala Future. A naive approach at looking how they could be incorporated into Erlang.

-module(future).
-compile(export_all).

test() -> test(self()).
test(ClientPid) -> 
	application:start(inets),
	spawn(fun() -> future([httpGet("http://www.google.com"), httpGet("http://www.bing.com"), httpGet("http://www.amazon.com")], 
		ClientPid) end),
	receive
		HttpResponsHeaders -> handle_results(HttpResponsHeaders, [])
	end.


%% extracts server
handle_results([], Results) -> Results;
handle_results([{Url, HttpRespHeaders}|Rest], Results) -> handle_results(Rest, Results ++ [{Url, header(headers(HttpRespHeaders), "server")}]). 



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



httpGet(Url) -> fun() -> {Url, httpc:request(Url)} end.

headers({ok, {Status, Headers, Body}}) -> Headers.
header([{Header, Value}|Tail], Header) -> {Header,Value};
header([{_, Value}|Tail], Header) -> header(Tail,Header).



%% mock a webservice lookup
ws_lookup() -> {ws, "web service response"}.
db_lookup() -> {db, "database response"}.
