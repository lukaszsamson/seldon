-module(stream_registry).
-export([startStreamRegistry/1]).

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
              NewStream = stream:startStream(StreamId, Events, Store),
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
  spawn(fun () -> streamRegistry(Store) end).
