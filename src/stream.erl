-module(stream).
-include("common.hrl").
-export([stream/3]).

-spec isVersionOk(non_neg_integer(), integer()) -> boolean().
isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

-spec getVersion(list(event())) -> integer().
getVersion(Events) -> length(Events).

-spec stream(stream_id(), list(event()), store()) -> no_return().
stream(Id, Events, Store) ->
  stream(Id, Events, Store, []).

-spec stream(stream_id(), list(event()), store(), list(observer())) -> no_return().
stream(Id, Events, Store, Observers) ->
  Version = getVersion(Events),
  receive
    stop -> ok;
    {From, observe} when is_pid(From) ->
      stream(Id, Events, Store, Observers ++ [From]);
    {From, unobserve} when is_pid(From) ->
      stream(Id, Events, Store, Observers -- [From]);
    {From, getEvents} when is_pid(From) ->
      From ! Events,
      stream(Id, Events, Store, Observers);
    {From, getVersion} when is_pid(From) ->
      From ! Version,
      stream(Id, Events, Store, Observers);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
      case isVersionOk(Version, MaxVersion) of
        true ->
          UpdatedEvents = Events ++ NewEvents,
          Store ! {self(), save, Id, UpdatedEvents},
          receive
            ok ->
              From ! ack,
              lists:foreach(fun (O) -> O ! NewEvents end, Observers),
              stream(Id, UpdatedEvents, Store, Observers);
            Error ->
              From ! Error,
              stream(Id, Events, Store, Observers)
          after
            % TODO config
            100 ->
              From ! timeout,
              stream(Id, Events, Store, Observers)
          end;
        false ->
          From ! concurrencyError,
          stream(Id, Events, Store, Observers)
      end
  after
    % TODO config
    500 -> ok
  end.
