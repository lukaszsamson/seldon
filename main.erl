-module(main).
-export([stream/1]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

stream(Events) ->
  io:format("Listening~n", []),
  Version = length(Events),
  receive
    {From, getEvents} ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Events);
    {From, getVersion} ->
      io:format("getVersion rec~n", []),
      From ! Version,
      stream(Events);
    {From, appendEvents, NewEvents, MaxVersion} ->
      io:format("appendEvents rec~n", []),
      case isVersionOk(Version, MaxVersion) of
        true ->
          From ! ack,
          stream(Events ++ NewEvents);
        false ->
          From ! concurrencyError,
          stream(Events)
      end
  end.
