-module(stream_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-import(common_mocks, [startMockStore/0, startMockStore/2]).

startMockStream(_, InitialEvents, _) ->
  MockStream = fun MockStream() ->
    receive
      {Sender, getEvents} ->
        Sender ! InitialEvents,
        MockStream();
      {Sender, getVersion} ->
        Sender ! 0,
        MockStream();
      _ -> MockStream()
    end
  end,
  MockStream().

startStreamRegistry(Store, StartStream) ->
  spawn_link(fun () -> stream_registry:streamRegistry(Store, StartStream) end).

streamRegistry_getStreams_should_return_initial_empty_test() ->
 R = startStreamRegistry(startMockStore(), fun startMockStream/3),
 R ! {self(), getStreams},
 receive
   Streams ->
     ?assertEqual(#{}, Streams)
 end.

streamRegistry_getStream_should_start_new_stream_test() ->
  R = startStreamRegistry(startMockStore(), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  receive
    {ok, Stream} ->
      ?assert(is_pid(Stream))
  end.

streamRegistry_getStream_should_load_from_store_test() ->
  R = startStreamRegistry(startMockStore(ok, [1]), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  S ! {self(), getEvents},
  receive
    Events ->
      ?assertEqual([1], Events)
  end.

streamRegistry_getStreams_should_return_newly_started_stream_empty_test() ->
  R = startStreamRegistry(startMockStore(), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  R ! {self(), getStreams},
  receive
    Streams ->
      ?assertEqual(#{"test" => S}, Streams)
  end.

streamRegistry_getStream_should_start_a_valid_stream_test() ->
  R = startStreamRegistry(startMockStore(), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  S ! {self(), getVersion},
  receive
    Version ->
      ?assertEqual(0, Version)
  end.

streamRegistry_getStream_should_return_existing_stream_test() ->
  R = startStreamRegistry(startMockStore(), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  S1 = receive
    {ok, Stream1} -> Stream1
  end,
  R ! {self(), getStream, "test"},
  S2 = receive
    {ok, Stream2} -> Stream2
  end,
  ?assertEqual(S1, S2).

streamRegistry_getStreams_should_not_return_stopped_stream_test() ->
  R = startStreamRegistry(startMockStore(), fun startMockStream/3),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  exit(S, error),
  receive after 1 -> ok end,
  R ! {self(), getStreams},
  receive
    Streams ->
      ?assertEqual(#{}, Streams)
  end.