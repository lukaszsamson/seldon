-module(main).
-export([bootstrap/0]).

bootstrap() ->
  Store = spawn_link(fun () -> store:store() end),
  _ = spawn_link(fun () -> stream_registry:streamRegistry(Store, fun stream:stream/3) end),
  receive
  after
    10 -> ok
  end.
