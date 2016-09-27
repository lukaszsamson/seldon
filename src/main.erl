-module(main).
-include("common.hrl").
-export([bootstrap/0, shutdown/1]).

-spec bootstrap() -> {store(), pid()}.
bootstrap() ->
  Store = store:start_link(),
  Registry = stream_registry:start_link(Store, fun stream:init/3),
  {Store, Registry}.

-spec shutdown({store(), pid()}) -> ok.
shutdown({Store, Registry}) ->
  Registry ! stop,
  Store ! stop,
  ok.
