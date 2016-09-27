-module(stream).
-include("common.hrl").
-export([start/3, start_link/3, init/3]).

-spec start(stream_id(), list(event()), store()) -> pid().
start(Id, Events, Store) ->
  spawn(?MODULE, init, [Id, Events, Store]).

-spec start_link(stream_id(), list(event()), store()) -> pid().
start_link(Id, Events, Store) ->
  spawn_link(?MODULE, init, [Id, Events, Store]).

-spec init(stream_id(), list(event()), store()) -> no_return().
init(Id, Events, Store) ->
  loop(Id, Events, Store).

-spec isVersionOk(non_neg_integer(), integer()) -> boolean().
isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

-spec getVersion(list(event())) -> integer().
getVersion(Events) -> length(Events).

-spec loop(stream_id(), list(event()), store()) -> no_return().
loop(Id, Events, Store) ->
  loop(Id, Events, Store, []).

-spec loop(stream_id(), list(event()), store(), list(observer())) -> no_return().
loop(Id, Events, Store, Observers) ->
  Version = getVersion(Events),
  receive
    stop -> ok;
    {From, observe} when is_pid(From) ->
      loop(Id, Events, Store, Observers ++ [From]);
    {From, unobserve} when is_pid(From) ->
      loop(Id, Events, Store, Observers -- [From]);
    {From, getEvents} when is_pid(From) ->
      From ! Events,
      loop(Id, Events, Store, Observers);
    {From, getVersion} when is_pid(From) ->
      From ! Version,
      loop(Id, Events, Store, Observers);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
      case isVersionOk(Version, MaxVersion) of
        true ->
          UpdatedEvents = Events ++ NewEvents,
          Store ! {self(), save, Id, UpdatedEvents},
          receive
            ok ->
              From ! ack,
              lists:foreach(fun (O) -> O ! NewEvents end, Observers),
              loop(Id, UpdatedEvents, Store, Observers);
            Error ->
              From ! Error,
              loop(Id, Events, Store, Observers)
          after
            % TODO config
            100 ->
              From ! timeout,
              loop(Id, Events, Store, Observers)
          end;
        false ->
          From ! concurrencyError,
          loop(Id, Events, Store, Observers)
      end
  after
    % TODO config
    500 -> ok
  end.
