-module(stream_tests).
-include_lib("eunit/include/eunit.hrl").
-import(common_mocks, [startMockStore/0, startMockStore/2, stopMockStore/1]).

startStream(Id, InitialEvents, Store) ->
  spawn(fun () -> stream:stream(Id, InitialEvents, Store) end).

stream_should_stop_if_no_messages_test_() -> {
    setup,
    fun () ->
      MockStore = common_mocks:startMockStore(),
      {Stream, Ref} = spawn_monitor(fun () -> stream:stream("id", [], MockStore) end),
      {MockStore, Stream, Ref}
    end,
    fun ({MockStore, Stream, Ref}) ->
      erlang:demonitor(Ref, [flush]),
      Stream ! stop,
      common_mocks:stopMockStore(MockStore)
    end,
    fun ({_, _, _}) ->
      R = receive
        {'DOWN', _, process, _, normal} -> ok
      end,
      [?_assertEqual(ok, R)]
    end
  }.

startStreamAndStore() ->
  startStreamAndStore([]).

startStreamAndStore(InitialEvents) ->
  MockStore = common_mocks:startMockStore(),
  Stream = startStream("id1", InitialEvents, MockStore),
  {MockStore, Stream}.

startStreamAndStoreError() ->
  MockStore = common_mocks:startMockStore(error, []),
  Stream = startStream("id1", [], MockStore),
  {MockStore, Stream}.

stopStreamAndStore({MockStore, Stream}) ->
  Stream ! stop,
  common_mocks:stopMockStore(MockStore).

stopStreamAndStore(_, {MockStore, Stream}) ->
  stopStreamAndStore({MockStore, Stream}).

stream_init_test_() ->
  Test = fun (InitialVersion) ->
    fun (InitialEvents, {_, Stream}) -> [
      {"check events", begin
        Stream ! {self(), getEvents},
        receive
          Events -> [?_assertEqual(InitialEvents, Events)]
        end
      end},
      {"check version", begin
        Stream ! {self(), getVersion},
        receive
          Version -> [?_assertEqual(InitialVersion, Version)]
        end
      end}]
    end
  end,
  {
    foreachx,
    fun startStreamAndStore/1,
    fun stopStreamAndStore/2,
    [{X, Test(V)} || {X, V} <- [{[], 0}, {[1, 2, 3], 3}]]
  }.

appendEvents_should_return_error_if_not_stored_test_() -> {
    setup,
    fun startStreamAndStoreError/0,
    fun stopStreamAndStore/1,
    fun ({_, Stream}) ->
      % Stream ! {self(), observe},
      Stream ! {self(), appendEvents, [1], 0},
      receive
        Response -> ?_assertEqual(error, Response)
      end
    end
  }.

appendEvents_test_() ->
  Test = fun (Appended, MaxVersion, Expected, ExpectedVersion, ExpectedResult) ->
    fun (_, {_, Stream}) ->
      Stream ! {self(), appendEvents, Appended, MaxVersion},
      Result = receive
        R -> R
      end,
      [
      {"check result", ?_assertEqual(ExpectedResult, Result)},
      {"check events", begin
        Stream ! {self(), getEvents},
        receive
          Events -> ?_assertEqual(Expected, Events)
        end
      end},
      {"check version", begin
        Stream ! {self(), getVersion},
        receive
          Version -> ?_assertEqual(ExpectedVersion , Version)
        end
      end}]
    end
  end,
  {
    foreachx,
    fun startStreamAndStore/1,
    fun stopStreamAndStore/2,
    [{Initial, Test(Appended, MaxVersion, Expected, ExpectedVersion, ExpectedResult)} || {Initial, Appended, MaxVersion, Expected, ExpectedVersion, ExpectedResult} <- [
      {[], [5], -1, [5], 1, ack},
      {[], [5], 0, [5], 1, ack},
      {[2, 3], [5], -1, [2, 3, 5], 3, ack},
      {[2, 3], [5], 3, [2, 3, 5], 3, ack},
      {[2, 3], [5], 1, [2, 3], 2, concurrencyError}
      ]
    ]
  }.

observer_should_get_updates_test_() -> {
    setup,
    fun startStreamAndStore/0,
    fun stopStreamAndStore/1,
    fun ({_, Stream}) ->
      Stream ! {self(), observe},
      Go = fun (EventsToAppend) ->
        Stream ! {self(), appendEvents, EventsToAppend, -1},
        receive
          ack -> ok
        end,
        receive
          Events -> ?_assertEqual(EventsToAppend, Events)
        end
      end,
      [
      {"single", Go([1])},
      {"multiple", Go([1, 2])}
      ]
    end
  }.

observer_should_not_get_updates_on_error_test_() -> {
    setup,
    fun () -> startStreamAndStore([1]) end,
    fun stopStreamAndStore/1,
    fun ({_, Stream}) ->
      Stream ! {self(), observe},
      Stream ! {self(), appendEvents, [1], 0},
      receive
        concurrencyError -> ok
      end,
      R = receive
        _ -> true
      after
        10 -> false
      end,
      ?_assertEqual(false, R)
    end
  }.

observer_should_not_get_updates_on__store_error_test_() -> {
    setup,
    fun startStreamAndStoreError/0,
    fun stopStreamAndStore/1,
    fun ({_, Stream}) ->
      Stream ! {self(), observe},
      Stream ! {self(), appendEvents, [1], 0},
      receive
        error -> ok
      end,
      R = receive
        _ -> true
      after
        10 -> false
      end,
      ?_assertEqual(false, R)
    end
  }.

unobserve_should_end_subscription_test_() -> {
    setup,
    fun startStreamAndStore/0,
    fun stopStreamAndStore/1,
    fun ({_, Stream}) ->
      Stream ! {self(), observe},
      Stream ! {self(), unobserve},
      Stream ! {self(), appendEvents, [1], -1},
      receive
        ack -> ok
      end,
      R = receive
        _ -> true
      after
        10 -> false
      end,
      [?_assertEqual(false, R)]
    end
  }.

not_listening_observer_should_not_block_stream_test_() -> {
    setup,
    fun () ->
      MockStore = common_mocks:startMockStore(),
      Stream = spawn(fun () -> stream:stream("id", [], MockStore) end),
      Observer = spawn(fun () ->
        receive
          stop -> ok
        end
      end),
      {MockStore, Stream, Observer}
    end,
    fun ({MockStore, Stream, Observer}) ->
      Observer ! stop,
      Stream ! stop,
      common_mocks:stopMockStore(MockStore)
    end,
    fun ({_, Stream, _}) ->
      Stream ! {self(), appendEvents, [1], 0},
      R = receive
        ack -> ack
      end,
      [?_assertEqual(ack, R)]
    end
  }.
