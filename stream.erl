-module(stream).
-export([startStream/3]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

getVersion(Events) -> length(Events).

stream(Id, Events, Store, Observers) ->
  Version = getVersion(Events),
  receive
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
            100 ->
              From ! timeout,
              stream(Id, Events, Store, Observers)
          end;
        false ->
          From ! concurrencyError,
          stream(Id, Events, Store, Observers)
      end
  end.

startStream(Id, InitialEvents, Store) ->
  spawn(fun () -> stream(Id, InitialEvents, Store, []) end).
