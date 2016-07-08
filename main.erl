-module(main).
-export([start/0, ping/2, pong/0, stream/1]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("ping finished~n", []);

ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1, Pong_PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

stream(Events) ->
  io:format("Listening~n", []),
  receive
    {From, getEvents} ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Events);
    _ ->
      io:format("dupa rec~n", [])
  end.

start() ->
  Events = [1, 2],
  io:format("Spawning~n", []),
  S = spawn(main, stream, [Events]),
  io:format("Sending~n", []),
  S ! {self(), getEvents},
  io:format("Receiving~n", []),
  receive
    [] ->
      io:format("1~n", []);
    [_|_] ->
      io:format("2~n", []);
    _ ->
      io:format("0~n", [])
  end.
