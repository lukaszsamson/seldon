-module(store).
-include("common.hrl").
-export([start/1, start_link/1, init/1, start/0, start_link/0, init/0]).

-type store_state() :: #{stream_id() => list(event)}.

-spec start() -> pid().
start() ->
  spawn(?MODULE, init, []).

-spec start_link() -> pid().
start_link() ->
  spawn_link(?MODULE, init, []).

-spec start(store_state()) -> pid().
start(StreamsEvents) ->
  spawn(?MODULE, init, [StreamsEvents]).

-spec start_link(store_state()) -> pid().
start_link(StreamsEvents) ->
  spawn_link(?MODULE, init, [StreamsEvents]).

-spec init() -> no_return().
init() ->
  loop(#{}).

-spec init(store_state()) -> no_return().
init(StreamsEvents) ->
  loop(StreamsEvents).

-spec loop(store_state()) -> no_return().
loop(StreamsEvents) ->
  receive
    stop -> ok;
    {From, save, StreamId, Events} when is_pid(From); is_list(StreamId); is_list(Events) ->
      % TODO validate StreamId with io_lib:printable_unicode_list(Term) -> boolean()
      From ! ok,
      loop(StreamsEvents#{StreamId => Events});
    {From, load, StreamId} when is_pid(From); is_list(StreamId) ->
      Events = case (maps:find(StreamId, StreamsEvents)) of
        {ok, Value} -> Value;
        error -> []
      end,
      From ! Events,
      loop(StreamsEvents)
  end.
