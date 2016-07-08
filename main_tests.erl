-module(main_tests).

-include_lib("eunit/include/eunit.hrl").

getEvents_returns_initial(InitialEvents) ->
  S = spawn(main, stream, [InitialEvents]),
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

appendEvents(InitialEvents, NewEvents, ExpectedEvents, MaxVersion) ->
  S = spawn(main, stream, [InitialEvents]),
  S ! {self(), appendEvents, NewEvents, MaxVersion},
  receive
    _ -> ok
  end,
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= ExpectedEvents)
  end,
  ok.

appendEvents_empty_test() ->
  appendEvents([], [5], [5], -1).

appendEvents_nonempty_test() ->
  appendEvents([2, 3], [5], [2, 3, 5], -1).

appendEvents_nonempty_should_not_append_if_wrong_version_test() ->
  appendEvents([2, 3], [5], [2, 3], 1).

appendEventsVersionCheck(InitialEvents, NewEvents, MaxVersion, ExpectedResult) ->
  S = spawn(main, stream, [InitialEvents]),
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
