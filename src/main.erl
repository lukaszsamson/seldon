-module(main).
-export([bootstrap/0, shutdown/1]).

bootstrap() ->
  Store = spawn_link(fun () -> store:store() end),
  Registry = spawn_link(fun () -> stream_registry:streamRegistry(Store, fun stream:stream/3) end),
  {Store, Registry}.

shutdown({Store, Registry}) ->
  Registry ! stop,
  Store ! stop.
