-module(functions).
-compile(export_all).


increase(By) -> fun(Number) -> Number + By end.



logger() -> logger("TestApp", fun os:timestamp/0).

logger(ApplicationName, TimeFunc) -> 
	fun(Message) -> io:format("Time:~p Application:~p Message:~p ~n", [TimeFunc(), ApplicationName, Message]) end.


logger_process() -> spawn(fun() -> logger(logger()) end).

logger(Logger) ->
	receive
		Message -> Logger(Message)
	end.


%% sequence comprised of functions
seq(From, From) -> fun() -> From end;
seq(To, From) -> fun() -> {To, seq(To+1, From)} end.

%% obviously wouldn't be used in something real
cache(Function) -> fun(Arg0) -> cache(Function, get(Arg0), Arg0) end.

cache(Function, undefined, Arg0) -> 
	Results = Function(Arg0),
	put(Arg0, Results),
	Results;
cache(_, CachedValue, _) -> CachedValue.

cache_sleep() -> cache(fun sleep/1).

sleep(Millis) -> 
		receive
		after Millis -> Millis
	end.

test() -> start_process(fun(X) -> io:format("Arg=~p~n", [X]) end).

start_process(Function) -> spawn(fun() -> process(Function) end).

process(Function) ->
	receive 
		Arg -> Function(Arg)
	end.


context() ->
	put(millis, fun os:timestamp/0),
	put(log, logger("Example", get(millis))).
	

test_compose() -> compose(fun dec/1, fun dec/1).
inc(X) -> X+1.
dec(X) -> X-1.


compose(Function,Function2) -> fun(Arg) -> Function2(Function(Arg)) end.