-module(store_tests).
-include_lib("eunit/include/eunit.hrl").

spawnStore(InitialData) ->
  spawn_link(fun () -> store:store(InitialData) end).

store_should_return_empty_list_for_new_streamId_test() ->
  S = spawnStore(#{}),
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [])
  end.

store_should_return_events_list_for_known_streamId_test() ->
  S = spawnStore(#{"stream1" => [1]}),
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1])
  end.

store_should_store_events_list_for_new_streamId_test() ->
  S = spawnStore(#{}),
  S ! {self(), save, "stream1", [1]},
  receive
    ok -> ok
  end,
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1])
  end.

store_should_store_events_list_for_known_streamId_test() ->
  S = spawnStore(#{"stream1" => [1]}),
  S ! {self(), save, "stream1", [1, 2]},
  receive
    ok -> ok
  end,
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1, 2])
  end.
