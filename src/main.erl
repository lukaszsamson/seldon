-module(main).
-include("common.hrl").
-export([bootstrap/0, shutdown/1]).

-spec bootstrap() -> {store(), pid()}.
bootstrap() ->
  Store = spawn_link(fun () -> store:store() end),
  Registry = spawn_link(fun () -> stream_registry:streamRegistry(Store, fun stream:stream/3) end),
  {Store, Registry}.

-spec shutdown({store(), pid()}) -> ok.
shutdown({Store, Registry}) ->
  Registry ! stop,
  Store ! stop,
  ok.
