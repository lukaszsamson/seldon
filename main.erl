-module(main).
-export([startStream/1, startStreamRegistry/1, stream/1, streamRegistry/1]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

stream(Events) ->
  io:format("Listening~n", []),
  Version = length(Events),
  receive
    {From, getEvents} when is_pid(From) ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Events);
    {From, getVersion} when is_pid(From) ->
      io:format("getVersion rec~n", []),
      From ! Version,
      stream(Events);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
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

startStream(InitialEvents) ->
  io:format("Starting stream~n", []),
  spawn(main, stream, [InitialEvents]).

streamRegistry(Streams) ->
  receive
    {From, getStream, StreamId} when is_pid(From); is_atom(StreamId) ->
      {Stream, NewStreams} = case (maps:find(StreamId, Streams)) of
        {ok, Value} -> {Value, Streams};
        error ->
          NewStream = startStream([]),
          {NewStream, Streams#{StreamId => NewStream}}
      end,
      From ! Stream,
      streamRegistry(NewStreams);
    {From, getStreams} when is_pid(From) ->
      From ! Streams,
      streamRegistry(Streams)
  end.

startStreamRegistry(InitialStreams) ->
  io:format("Starting stream registry~n", []),
  spawn(main, streamRegistry, [InitialStreams]).
