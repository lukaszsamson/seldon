-module(stream_registry).
-export([startStreamRegistry/2]).

streamRegistry(Store, StartStream) ->
  streamRegistry(#{}, Store, StartStream).

streamRegistry(Streams, Store, StartStream) ->
  receive
    {From, getStream, StreamId} when is_pid(From); is_list(StreamId) ->
      {Response, NewStreams} = case (maps:find(StreamId, Streams)) of
        {ok, Value} -> {{ok, Value}, Streams};
        error ->
          case load(Store, StreamId) of
            {ok, Events} ->
              NewStream = StartStream(StreamId, Events, Store),
              {{ok, NewStream}, Streams#{StreamId => NewStream}};
            timeout ->
              {timeout, Streams}
          end
      end,
      From ! Response,
      streamRegistry(NewStreams, Store, StartStream);
    {From, getStreams} when is_pid(From) ->
      From ! Streams,
      streamRegistry(Streams, Store, StartStream)
  end.

load(Store, StreamId) ->
  Store ! {self(), load, StreamId},
  receive
    Events when is_list(Events) -> {ok, Events}
  after
    1000 -> timeout
  end.

startStreamRegistry(Store, StartStream) ->
  spawn(fun () -> streamRegistry(Store, StartStream) end).
