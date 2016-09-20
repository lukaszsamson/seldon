-module(main).
-export([startStream/2, startStreamRegistry/1, stream/3, streamRegistry/1, store/1, startStore/1]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

getVersion(Events) -> length(Events).

stream(Id, Events, Observers) ->
  io:format("Listening~n", []),
  Version = getVersion(Events),
  receive
    {From, observe} when is_pid(From) ->
      stream(Id, Events, Observers ++ [From]);
    {From, unobserve} when is_pid(From) ->
      stream(Id, Events, Observers -- [From]);
    {From, getEvents} when is_pid(From) ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Id, Events, Observers);
    {From, getVersion} when is_pid(From) ->
      io:format("getVersion rec~n", []),
      From ! Version,
      stream(Id, Events, Observers);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
      io:format("appendEvents rec~n", []),
      case isVersionOk(Version, MaxVersion) of
        true ->
          From ! ack,
          lists:foreach(fun (O) -> O ! NewEvents end, Observers),
          stream(Id, Events ++ NewEvents, Observers);
        false ->
          From ! concurrencyError,
          stream(Id, Events, Observers)
      end
  end.

startStream(Id, InitialEvents) ->
  io:format("Starting stream~n", []),
  spawn(main, stream, [Id, InitialEvents, []]).

streamRegistry(Streams) ->
  receive
    {From, getStream, StreamId} when is_pid(From); is_atom(StreamId) ->
      {Stream, NewStreams} = case (maps:find(StreamId, Streams)) of
        {ok, Value} -> {Value, Streams};
        error ->
          NewStream = startStream(StreamId, []),
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

store(StreamsEvents) ->
  receive
    {From, save, StreamId, Events} when is_pid(From); is_atom(StreamId); is_list(Events) ->
      From ! ok,
      store(StreamsEvents#{StreamId => Events});
    {From, load, StreamId} when is_pid(From); is_atom(StreamId) ->
      Events = case (maps:find(StreamId, StreamsEvents)) of
        {ok, Value} -> Value;
        error -> []
      end,
      From ! Events,
      store(StreamsEvents)
  end.

  startStore(StreamsEvents) ->
    io:format("Starting store~n", []),
    spawn(main, store, [StreamsEvents]).
