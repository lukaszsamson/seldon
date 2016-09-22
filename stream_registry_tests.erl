-module(stream_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-import(common_mocks, [startMockStore/0, startMockStore/2]).

streamRegistry_getStreams_should_return_initial_empty_test() ->
 R = stream_registry:startStreamRegistry(startMockStore()),
 R ! {self(), getStreams},
 receive
   Streams ->
     ?assert(Streams =:= #{})
 end.

streamRegistry_getStream_should_start_new_stream_test() ->
  R = stream_registry:startStreamRegistry(startMockStore()),
  R ! {self(), getStream, "test"},
  receive
    {ok, Stream} ->
      ?assert(is_pid(Stream))
  end.

streamRegistry_getStream_should_load_from_store_test() ->
  R = stream_registry:startStreamRegistry(startMockStore(ok, [1])),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  S ! {self(), getEvents},
  receive
    Events ->
      ?assert(Events =:= [1])
  end.

streamRegistry_getStreams_should_return_newly_started_stream_empty_test() ->
  R = stream_registry:startStreamRegistry(startMockStore()),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  R ! {self(), getStreams},
  receive
    Streams ->
      ?assert(#{"test" => S} =:= Streams)
  end.

streamRegistry_getStream_should_start_a_valid_stream_test() ->
  R = stream_registry:startStreamRegistry(startMockStore()),
  R ! {self(), getStream, "test"},
  S = receive
    {ok, Stream} -> Stream
  end,
  S ! {self(), getVersion},
  receive
    Version ->
      ?assert(Version =:= 0)
  end.

streamRegistry_getStream_should_return_existing_stream_test() ->
  R = stream_registry:startStreamRegistry(startMockStore()),
  R ! {self(), getStream, "test"},
  S1 = receive
    {ok, Stream1} -> Stream1
  end,
  R ! {self(), getStream, "test"},
  S2 = receive
    {ok, Stream2} -> Stream2
  end,
  ?assert(S1 =:= S2).
