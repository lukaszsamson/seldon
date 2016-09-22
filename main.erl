-module(main).
-export([bootstrap/0]).

bootstrap() ->
  Store = store:startStore(),
  _ = stream_registry:startStreamRegistry(Store, fun stream:startStream/3),
  receive
  after
    10 -> ok
  end.
