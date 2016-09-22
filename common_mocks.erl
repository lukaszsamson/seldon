-module(common_mocks).
-compile(export_all).

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
