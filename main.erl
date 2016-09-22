-module(main).
-export([startStream/3, startStreamRegistry/1, startStore/1]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

getVersion(Events) -> length(Events).

stream(Id, Events, Store, Observers) ->
  io:format("Listening~n", []),
  Version = getVersion(Events),
  receive
    {From, observe} when is_pid(From) ->
      stream(Id, Events, Store, Observers ++ [From]);
    {From, unobserve} when is_pid(From) ->
      stream(Id, Events, Store, Observers -- [From]);
    {From, getEvents} when is_pid(From) ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Id, Events, Store, Observers);
    {From, getVersion} when is_pid(From) ->
      io:format("getVersion rec~n", []),
      From ! Version,
      stream(Id, Events, Store, Observers);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
      io:format("appendEvents rec~n", []),
      case isVersionOk(Version, MaxVersion) of
        true ->
          UpdatedEvents = Events ++ NewEvents,
          Store ! {self(), save, Id, UpdatedEvents},
          receive
            ok ->
              From ! ack,
              lists:foreach(fun (O) -> O ! NewEvents end, Observers),
              stream(Id, UpdatedEvents, Store, Observers);
            Error ->
              From ! Error,
              stream(Id, Events, Store, Observers)
          after
            100 ->
              io:format("store timeout~n", []),
              From ! timeout,
              stream(Id, Events, Store, Observers)
          end;
        false ->
          From ! concurrencyError,
          stream(Id, Events, Store, Observers)
      end
  end.

startStream(Id, InitialEvents, Store) ->
  io:format("Starting stream~n", []),
  spawn(fun () -> stream(Id, InitialEvents, Store, []) end).

streamRegistry(Store) ->
  streamRegistry(#{}, Store).

streamRegistry(Streams, Store) ->
  receive
    {From, getStream, StreamId} when is_pid(From); is_list(StreamId) ->
      {Response, NewStreams} = case (maps:find(StreamId, Streams)) of
        {ok, Value} -> {{ok, Value}, Streams};
        error ->
          case load(Store, StreamId) of
            {ok, Events} ->
              NewStream = startStream(StreamId, Events, Store),
              {{ok, NewStream}, Streams#{StreamId => NewStream}};
            timeout ->
              {timeout, Streams}
          end
      end,
      From ! Response,
      streamRegistry(NewStreams, Store);
    {From, getStreams} when is_pid(From) ->
      From ! Streams,
      streamRegistry(Streams, Store)
  end.

load(Store, StreamId) ->
  Store ! {self(), load, StreamId},
  receive
    Events when is_list(Events) -> {ok, Events}
  after
    1000 -> timeout
  end.

startStreamRegistry(Store) ->
  io:format("Starting stream registry~n", []),
  spawn(fun () -> streamRegistry(Store) end).

store(StreamsEvents) ->
  receive
    {From, save, StreamId, Events} when is_pid(From); is_list(StreamId); is_list(Events) ->
      % TODO validate StreamId with io_lib:printable_unicode_list(Term) -> boolean()
      From ! ok,
      store(StreamsEvents#{StreamId => Events});
    {From, load, StreamId} when is_pid(From); is_list(StreamId) ->
      Events = case (maps:find(StreamId, StreamsEvents)) of
        {ok, Value} -> Value;
        error -> []
      end,
      From ! Events,
      store(StreamsEvents)
  end.

  startStore(StreamsEvents) ->
    io:format("Starting store~n", []),
    spawn(fun () -> store(StreamsEvents) end).
