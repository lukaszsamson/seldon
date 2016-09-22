-module(common_mocks).
-export([startMockStore/0, startMockStore/2]).

startMockStore() ->
  startMockStore(ok, []).

startMockStore(Result, InitialEvents) ->
  spawn(fun MockStore() ->
    receive
      {Sender, save, _, _} ->
        Sender ! Result,
        MockStore();
      {Sender, load, _} ->
        Sender ! InitialEvents,
        MockStore()
    end
  end).
  
