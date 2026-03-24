-module(processes).
-export([ping/1, pong/0, start/0]).

ping(0) ->
    pong ! finished,
    io:format("Ping finished~n");
ping(N) ->
    pong ! {self(), ping},
    receive
        pong -> io:format("Ping received pong~n")
    end,
    ping(N - 1).

pong() ->
    receive
        finished -> io:format("Pong finished~n");
        {From, ping} ->
            io:format("Pong received ping~n"),
            From ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(fun pong/0)),
    ping(3).
