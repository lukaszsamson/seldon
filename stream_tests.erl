-module(stream_tests).
-include_lib("eunit/include/eunit.hrl").
-import(common_mocks, [startMockStore/0, startMockStore/2]).

startStream(Id, InitialEvents, Store) ->
  spawn_link(fun () -> stream:stream(Id, InitialEvents, Store) end).

getEvents_returns_initial(InitialEvents) ->
  S = startStream("id1", InitialEvents, startMockStore()),
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= InitialEvents)
  end.

getEvents_returns_initial_empty_test() ->
  getEvents_returns_initial([]).

getEvents_returns_initial_nonempty_test() ->
  getEvents_returns_initial([2, 3, 5]).

getVersion_returns_initial(InitialEvents, ExpectedResult) ->
  S = startStream("id1", InitialEvents, startMockStore()),
  S ! {self(), getVersion},
  receive
    Version ->
      ?assert(Version =:= ExpectedResult)
  end.

getVersion_returns_initial_empty_test() ->
  getVersion_returns_initial([], 0).

getVersion_returns_initial_nonempty_test() ->
  getVersion_returns_initial([2, 3, 5], 3).

appendEvents_should_return_error_if_not_stored_test() ->
  S = startStream("id1", [], startMockStore(error, [])),
  S ! {self(), appendEvents, [1], 0},
  receive
    Response -> ?assert(Response =:= error)
  end.

appendEventsCheckCommon(InitialEvents, NewEvents, MaxVersion) ->
  S = startStream("id1", InitialEvents, startMockStore()),
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
  end.

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
  end.

appendEvents_empty_should_increase_version_test() ->
  appendEventsCheckVersion([], [5], 1, -1).

appendEvents_nonempty_increase_version_test() ->
  appendEventsCheckVersion([2, 3], [5], 3, -1).

appendEvents_nonempty_should_not_increase_version_if_wrong_version_test() ->
  appendEventsCheckVersion([2, 3], [5], 2, 1).

appendEventsVersionCheck(InitialEvents, NewEvents, MaxVersion, ExpectedResult) ->
  S = startStream("id1", InitialEvents, startMockStore()),
  S ! {self(), appendEvents, NewEvents, MaxVersion},
  receive
    Result -> ?assert(Result =:= ExpectedResult)
  end.

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

observer_should_get_updates_test() ->
  S = startStream("id1", [], startMockStore()),
  S ! {self(), observe},
  S ! {self(), appendEvents, [1], 0},
  receive
    _ -> ok
  end,
  receive
    Events -> ?assert(Events =:= [1])
  end.

observer_should_get_updates_multiple_test() ->
  S = startStream("id1", [], startMockStore()),
  S ! {self(), observe},
  S ! {self(), appendEvents, [1], 0},
  receive
    _ -> ok
  end,
  receive
    Events -> ?assert(Events =:= [1])
  end,
  S ! {self(), appendEvents, [2, 3], 1},
  receive
    _ -> ok
  end,
  receive
    Events1 -> ?assert(Events1 =:= [2, 3])
  end.

unobserve_should_end_subscription_test() ->
  S = startStream("id1", [], startMockStore()),
  S ! {self(), observe},
  S ! {self(), unobserve},
  S ! {self(), appendEvents, [1], 0},
  receive
    _ -> ok
  end,
  receive
    _ -> ?assert(false)
  after
    1 -> ok
  end.

observer_should_get_updates_multiple_queued_test() ->
  S = startStream("id1", [], startMockStore()),
  Self = self(),
  O = spawn(fun () ->
    receive
    after
      10 -> ok
    end,
    receive
      Events -> ?assert(Events =:= [1])
    end,
    receive
      Events1 -> ?assert(Events1 =:= [2, 3])
    end,
    Self ! ok
  end),
  S ! {O, observe},

  S ! {self(), appendEvents, [1], 0},
  receive
    _ -> ok
  end,
  S ! {self(), appendEvents, [2, 3], 1},
  receive
    ack -> ok
  end,
  receive
    ok -> ok
  end.

observer_should_not_kill_stream_test() ->
  S = startStream("id1", [], startMockStore()),
  O = spawn(fun () ->
    receive
      Events -> ?assert(Events =:= [1])
    end
  end),
  S ! {O, observe},

  S ! {self(), appendEvents, [1], 0},
  receive
    _ -> ok
  end,
  S ! {self(), appendEvents, [2, 3], 1},
  receive
    ack -> ok
  end,
  S ! {self(), getVersion},
  receive
    Version ->
      ?assert(Version =:= 3)
  end.
