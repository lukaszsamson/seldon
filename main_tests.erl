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

appendEvents(InitialEvents, NewEvents) ->
  S = spawn(main, stream, [InitialEvents]),
  S ! {self(), appendEvents, NewEvents},
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= InitialEvents ++ NewEvents)
  end,
  ok.

appendEvents_empty_test() ->
  appendEvents([], [5]).

appendEvents_nonempty_test() ->
  appendEvents([2, 3], [5]).
