-module(stream).
-export([startStream/3]).

isVersionOk(Version, MaxVersion) ->
  MaxVersion < 0 orelse Version =< MaxVersion.

getVersion(Events) -> length(Events).

stream(Id, Events, Store, Observers) ->
  io:format("Listening~n", []),
  Version = getVersion(Events),
  receive
    {From, observe} when is_pid(From) ->
      stream(Id, Events, Store, Observers ++ [From]);
    {From, unobserve} when is_pid(From) ->
      stream(Id, Events, Store, Observers -- [From]);
    {From, getEvents} when is_pid(From) ->
      io:format("getEvents rec~n", []),
      From ! Events,
      stream(Id, Events, Store, Observers);
    {From, getVersion} when is_pid(From) ->
      io:format("getVersion rec~n", []),
      From ! Version,
      stream(Id, Events, Store, Observers);
    {From, appendEvents, NewEvents, MaxVersion} when is_pid(From); is_list(NewEvents); is_integer(MaxVersion) ->
      io:format("appendEvents rec~n", []),
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
              io:format("store timeout~n", []),
              From ! timeout,
              stream(Id, Events, Store, Observers)
          end;
        false ->
          From ! concurrencyError,
          stream(Id, Events, Store, Observers)
      end
  end.

startStream(Id, InitialEvents, Store) ->
  io:format("Starting stream~n", []),
  spawn(fun () -> stream(Id, InitialEvents, Store, []) end).
