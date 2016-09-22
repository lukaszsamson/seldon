-module(store).
-export([startStore/0, startStore/1]).

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

startStore() ->
  startStore(#{}).

startStore(StreamsEvents) ->
  io:format("Starting store~n", []),
  spawn(fun () -> store(StreamsEvents) end).
