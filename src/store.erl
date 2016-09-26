-module(store).
-include("common.hrl").
-export([store/0, store/1]).

-spec store() -> no_return().
store() ->
  store(#{}).

-spec store(#{stream_id() => list(event)}) -> no_return().
store(StreamsEvents) ->
  receive
    stop -> ok;
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
