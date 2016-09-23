-module(stream_registry).
-export([streamRegistry/2]).

streamRegistry(Store, StartStream) ->
  streamRegistry(#{}, Store, StartStream).

streamRegistry(Streams, Store, StartStream) ->
  receive
    stop ->
      % TODO link
      lists:foreach(fun (O) -> O ! stop end, maps:values(Streams)),
      ok;
    {From, getStream, StreamId} when is_pid(From); is_list(StreamId) ->
      % TODO validate StreamId with io_lib:printable_unicode_list(Term) -> boolean()
      {Response, NewStreams} = case (maps:find(StreamId, Streams)) of
        {ok, Value} -> {{ok, Value}, Streams};
        error ->
          case load(Store, StreamId) of
            {ok, Events} ->
              {NewStream, _} = spawn_monitor(fun () ->
                StartStream(StreamId, Events, Store)
              end),
              {{ok, NewStream}, Streams#{StreamId => NewStream}};
            timeout ->
              {timeout, Streams}
          end
      end,
      From ! Response,
      streamRegistry(NewStreams, Store, StartStream);
    {From, getStreams} when is_pid(From) ->
      From ! Streams,
      streamRegistry(Streams, Store, StartStream);
    {'DOWN', _, process, Pid, _} ->
      Ks = [K || {K, V} <- maps:to_list(Streams), V =:= Pid],
      streamRegistry(maps:without(Ks, Streams), Store, StartStream)
  end.

load(Store, StreamId) ->
  Store ! {self(), load, StreamId},
  receive
    Events when is_list(Events) -> {ok, Events}
  after
    % TODO config
    1000 -> timeout
  end.
