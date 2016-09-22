-module(store_tests).
-include_lib("eunit/include/eunit.hrl").

store_should_return_empty_list_for_new_streamId_test() ->
  S = store:startStore(#{}),
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [])
  end.

store_should_return_events_list_for_known_streamId_test() ->
  S = store:startStore(#{"stream1" => [1]}),
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1])
  end.

store_should_store_events_list_for_new_streamId_test() ->
  S = store:startStore(#{}),
  S ! {self(), save, "stream1", [1]},
  receive
    ok -> ok
  end,
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1])
  end.

store_should_store_events_list_for_known_streamId_test() ->
  S = store:startStore(#{"stream1" => [1]}),
  S ! {self(), save, "stream1", [1, 2]},
  receive
    ok -> ok
  end,
  S ! {self(), load, "stream1"},
  receive
    Events -> ?assert(Events =:= [1, 2])
  end.
