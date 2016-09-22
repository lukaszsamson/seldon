-module(crdt_set).
-export([go/0]).

synchronize(State, Replicas) ->
  lists:foreach(fun (R) -> R ! {synch, State} end, Replicas).

replica(Id) ->
  replica(Id, sets:new(), []).

replica(Id, State, Replicas) ->
  io:format("Replica ~w: ~50p~n", [Id, sets:to_list(State)]),
  receive
    {add, E} ->
      NewState = sets:add_element(E, State),
      synchronize(NewState, Replicas),
      replica(Id, NewState, Replicas);
    {synch, RemoteState} ->
      NewState = sets:union(State, RemoteState),
      replica(Id, NewState, Replicas);
    {setReplcas, NewReplicas} ->
      replica(Id, State, NewReplicas);
    done ->
      io:format("Shutdown ~w~n", [Id]),
      ok
  after
    rand:uniform(500) + 200 ->
      synchronize(State, Replicas),
      replica(Id, State, Replicas)
  end.

proxy(To) ->
  receive
    Message ->
      receive
      after
        rand:uniform(200) + 20 -> ok
      end,
      case rand:uniform(4) of
        X when X > 1 ->
          io:format("Dropping ~w to ~w~n", [Message, To]);
        1 ->
          io:format("Proxying ~w to ~w~n", [Message, To]),
          To ! Message
      end,
      proxy(To)
  end.

go() ->
  io:format("Starting replicas~n"),
  Replicas = [spawn(fun () -> replica(Id) end) || Id <- lists:seq(1, 3)],
  ProxiedReplicas = maps:from_list([{Replica, spawn(fun () -> proxy(Replica) end)} || Replica <- Replicas]),
  io:format("Updating replicas~n"),
  lists:foreach(fun (R) -> R ! {setReplcas, maps:values(maps:without([R], ProxiedReplicas))} end, Replicas),
  lists:nth(rand:uniform(length(Replicas)), Replicas) ! {add, 4},
  lists:nth(rand:uniform(length(Replicas)), Replicas) ! {add, 5},
  io:format("Waiting~n"),
  receive
  after
    5000 ->
      io:format("Shutdown~n"),
      lists:foreach(fun (R) ->
        R ! done
      end, Replicas)
  end,
  io:format("Quiting~n")
  .
