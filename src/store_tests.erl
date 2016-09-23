-module(store_tests).
-include_lib("eunit/include/eunit.hrl").

spawnStore(InitialData) ->
  spawn(fun () -> store:store(InitialData) end).

stopStore(_, Store) ->
  Store ! stop.

store_load_should_return_events_for_streamId_test_() ->
  Test = fun (ExpectedEvents) ->
    fun (_, Store) ->
      Store ! {self(), load, "stream1"},
      receive
        Events -> ?_assertEqual(ExpectedEvents, Events)
      end
    end
  end,
  {
    foreachx,
    fun spawnStore/1,
    fun stopStore/2,
    [{InitialData, Test(ExpectedEvents)} || {InitialData, ExpectedEvents} <- [
      {#{}, []},
      {#{"stream1" => [1]}, [1]}
    ]]
  }.

store_save_should_persist_events_for_streamId_test_() ->
  Test = fun (ExpectedEvents) ->
    fun (_, Store) ->
      Store ! {self(), save, "stream1", [1, 2]},
      receive
        ok -> ok
      end,
      Store ! {self(), load, "stream1"},
      receive
        Events -> ?_assertEqual(ExpectedEvents, Events)
      end
    end
  end,
  {
    foreachx,
    fun spawnStore/1,
    fun stopStore/2,
    [{InitialData, Test(ExpectedEvents)} || {InitialData, ExpectedEvents} <- [
      {#{}, [1, 2]},
      {#{"stream1" => [1]}, [1, 2]}
    ]]
  }.
