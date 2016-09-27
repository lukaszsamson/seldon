-module(stream_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-import(common_mocks, [startMockStore/0, startMockStore/2, stopMockStore/1]).

startMockStream(_, InitialEvents, _) ->
  MockStream = fun MockStream() ->
    receive
      stop -> ok;
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

startAll() ->
  Store = common_mocks:startMockStore(),
  Registry = stream_registry:start(Store, fun startMockStream/3),
  {Store, Registry}.

stopAll({Store, Registry}) ->
  Registry ! stop,
  common_mocks:stopMockStore(Store).

streamRegistry_getStreams_should_return_initial_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStreams},
       receive
         Streams -> ?_assertEqual(#{}, Streams)
       end
      end]
    end
  }.

streamRegistry_getStream_should_start_new_stream_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStream, "test"},
       receive
         {ok, Stream} -> ?_assert(is_pid(Stream))
       end
      end]
    end
  }.

streamRegistry_stop_should_stop_streams_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        S = receive
         {ok, Stream} -> Stream
        end,
        erlang:monitor(process, S),
        Registry ! stop,
        R = receive
         {'DOWN', _, process, _, normal} -> ok
        after 10 -> fail
        end,
        ?_assertEqual(ok, R)
      end]
    end
  }.

streamRegistry_kill_should_stop_streams_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        S = receive
         {ok, Stream} -> Stream
        end,
        erlang:monitor(process, S),
        exit(Registry, kill),
        R = receive
          {'DOWN', _, process, _, _} -> ok
        after 10 -> fail
        end,
        ?_assertEqual(ok, R)
      end]
    end
  }.

streamRegistry_getStream_should_load_from_store_test_() -> {
    setup,
    fun () ->
      Store = common_mocks:startMockStore(ok, [1]),
      Registry = stream_registry:start(Store, fun startMockStream/3),
      {Store, Registry}
    end,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStream, "test"},
       S = receive
         {ok, Stream} -> Stream
       end,
       S ! {self(), getEvents},
       receive
         Events -> ?_assertEqual([1], Events)
       end
      end]
    end
  }.

streamRegistry_getStreams_should_return_newly_started_stream_empty_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStream, "test"},
       Stream = receive
         {ok, S} -> S
       end,
       Registry ! {self(), getStreams},
       receive
         Streams -> ?_assertEqual(#{"test" => Stream}, Streams)
       end
      end]
    end
  }.

streamRegistry_getStream_should_return_existing_stream_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStream, "test"},
       S1 = receive
         {ok, Stream1} -> Stream1
       end,
       Registry ! {self(), getStream, "test"},
       S2 = receive
         {ok, Stream2} -> Stream2
       end,
       ?_assertEqual(S1, S2)
      end]
    end
  }.

streamRegistry_getStreams_should_not_return_stopped_stream_test_() -> {
    setup,
    fun startAll/0,
    fun stopAll/1,
    fun ({_, Registry}) -> [
      begin
       Registry ! {self(), getStream, "test"},
       S = receive
         {ok, Stream} -> Stream
       end,
       exit(S, error),
       receive after 1 -> ok end,
       Registry ! {self(), getStreams},
       receive
         Streams -> ?_assertEqual(#{}, Streams)
       end
      end]
    end
  }.
