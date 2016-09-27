-module(stream_registry).
-include("common.hrl").
-export([start/2, start_link/2, init/2]).

-spec start(store(), stream_ctor()) -> pid().
start(Store, StartStream) ->
  spawn(?MODULE, init, [Store, StartStream]).

-spec start_link(store(), stream_ctor()) -> pid().
start_link(Store, StartStream) ->
  spawn_link(?MODULE, init, [Store, StartStream]).

-spec init(store(), stream_ctor()) -> no_return().
init(Store, StartStream) ->
  process_flag(trap_exit, true),
  loop(#{}, Store, StartStream).

-spec loop(#{stream_id() => list(event())}, store(), stream_ctor()) -> no_return().
loop(Streams, Store, StartStream) ->
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
              NewStream = spawn_link(fun () ->
                StartStream(StreamId, Events, Store)
              end),
              {{ok, NewStream}, Streams#{StreamId => NewStream}};
            timeout ->
              {timeout, Streams}
          end
      end,
      From ! Response,
      loop(NewStreams, Store, StartStream);
    {From, getStreams} when is_pid(From) ->
      From ! Streams,
      loop(Streams, Store, StartStream);
    {'EXIT', Pid, _} ->
      Ks = [K || {K, V} <- maps:to_list(Streams), V =:= Pid],
      loop(maps:without(Ks, Streams), Store, StartStream)
  end.

-spec load(store(), stream_id()) -> {ok, list(event())} | timeout.
load(Store, StreamId) ->
  Store ! {self(), load, StreamId},
  receive
    Events when is_list(Events) -> {ok, Events}
  after
    % TODO config
    1000 -> timeout
  end.
