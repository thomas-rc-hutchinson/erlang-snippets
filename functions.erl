-module(functions).
-compile(export_all).


increase(By) -> fun(Number) -> Number + By end.


logger() -> logger("TestApp", fun os:timestamp/0).

logger(ApplicationName, TimeFunc) -> 
	fun(Message) -> io:format("Time:~p Application:~p Message:~p ~n", [TimeFunc(), ApplicationName, Message]) end.