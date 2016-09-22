-module(main).
-export([bootstrap/0]).

bootstrap() ->
  Store = store:startStore(),
  _ = stream_registry:startStreamRegistry(Store),
  receive
  after
    10 -> ok
  end.
