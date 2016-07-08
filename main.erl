-module(main).
-export([stream/1]).

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
  end.
