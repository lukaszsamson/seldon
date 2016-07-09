-module(main_tests).

-include_lib("eunit/include/eunit.hrl").

getEvents_returns_initial(InitialEvents) ->
  S = main:startStream(InitialEvents),
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= InitialEvents)
  end,
  ok.

getEvents_returns_initial_empty_test() ->
  getEvents_returns_initial([]).

getEvents_returns_initial_nonempty_test() ->
  getEvents_returns_initial([2, 3, 5]).

getVersion_returns_initial(InitialEvents, ExpectedResult) ->
  S = main:startStream(InitialEvents),
  S ! {self(), getVersion},
  receive
    Version ->
      ?assert(Version =:= ExpectedResult)
  end,
  ok.

getVersion_returns_initial_empty_test() ->
  getVersion_returns_initial([], 0).

getVersion_returns_initial_nonempty_test() ->
  getVersion_returns_initial([2, 3, 5], 3).

appendEventsCheckCommon(InitialEvents, NewEvents, MaxVersion) ->
  S = main:startStream(InitialEvents),
  S ! {self(), appendEvents, NewEvents, MaxVersion},
  receive
    _ -> ok
  end,
  S.

appendEventsCheckEvents(InitialEvents, NewEvents, ExpectedEvents, MaxVersion) ->
  S = appendEventsCheckCommon(InitialEvents, NewEvents, MaxVersion),
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= ExpectedEvents)
  end,
  ok.

appendEvents_empty_should_append_test() ->
  appendEventsCheckEvents([], [5], [5], -1).

appendEvents_nonempty_should_append_test() ->
  appendEventsCheckEvents([2, 3], [5], [2, 3, 5], -1).

appendEvents_nonempty_should_not_append_if_wrong_version_test() ->
  appendEventsCheckEvents([2, 3], [5], [2, 3], 1).

appendEventsCheckVersion(InitialEvents, NewEvents, ExpectedVersion, MaxVersion) ->
  S = appendEventsCheckCommon(InitialEvents, NewEvents, MaxVersion),
  S ! {self(), getVersion},
  receive
    Version ->
      ?assert(Version =:= ExpectedVersion)
  end,
  ok.

appendEvents_empty_should_increase_version_test() ->
  appendEventsCheckVersion([], [5], 1, -1).

appendEvents_nonempty_increase_version_test() ->
  appendEventsCheckVersion([2, 3], [5], 3, -1).

appendEvents_nonempty_should_not_increase_version_if_wrong_version_test() ->
  appendEventsCheckVersion([2, 3], [5], 2, 1).

appendEventsVersionCheck(InitialEvents, NewEvents, MaxVersion, ExpectedResult) ->
  S = main:startStream(InitialEvents),
  S ! {self(), appendEvents, NewEvents, MaxVersion},
  receive
    Result -> ?assert(Result =:= ExpectedResult)
  end,
  ok.

appendEvents_empty_anyVersion_test() ->
  appendEventsVersionCheck([], [5], -1, ack).

appendEvents_empty_exactVersion_test() ->
  appendEventsVersionCheck([], [5], 0, ack).

appendEvents_empty_greaterVersion_test() ->
  appendEventsVersionCheck([], [5], 1, ack).

appendEvents_nonempty_anyVersion_test() ->
  appendEventsVersionCheck([2, 3], [5], -1, ack).

appendEvents_nonempty_exactVersion_test() ->
  appendEventsVersionCheck([2, 3], [5], 2, ack).

appendEvents_nonempty_greaterVersion_test() ->
  appendEventsVersionCheck([2, 3], [5], 3, ack).

appendEvents_nonempty_lowerVersion_test() ->
  appendEventsVersionCheck([2, 3], [5], 1, concurrencyError).

streamRegistry_getStreams_should_return_initial_common(InitialStreams) ->
  R = main:startStreamRegistry(InitialStreams),
  R ! {self(), getStreams},
  receive
    Streams ->
      ?assert(Streams =:= InitialStreams)
  end,
  ok.

streamRegistry_getStreams_should_return_initial_empty_test() ->
 streamRegistry_getStreams_should_return_initial_common(#{}).

streamRegistry_getStreams_should_return_initial_nonempty_test() ->
  streamRegistry_getStreams_should_return_initial_common(#{'streamid' => 'stream'}).
