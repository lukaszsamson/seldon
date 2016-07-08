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
    {_From, appendEvents, NewEvents} ->
      io:format("appendEvents rec~n", []),
      stream(Events ++ NewEvents);
    _ ->
      io:format("dupa rec~n", []),
      stream(Events)
  end.

start() ->
  true.
