-module(main_tests).
-include_lib("eunit/include/eunit.hrl").

bootstrap_should_work_test_() -> {
    setup,
    fun main:bootstrap/0,
    fun main:shutdown/1,
    fun ({Store, Registry}) -> [
      ?_assert(is_pid(Store)),
      ?_assert(is_pid(Registry))
      ]
    end
  }.

start_stream_should_work_test_() -> {
    setup,
    fun main:bootstrap/0,
    fun main:shutdown/1,
    fun ({_, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        Stream = receive
          {ok, S} -> S
        end,
        ?_assert(is_pid(Stream))
      end
      ]
    end
  }.

append_to_stream_should_work_test_() -> {
    setup,
    fun main:bootstrap/0,
    fun main:shutdown/1,
    fun ({_, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        Stream = receive
          {ok, S} -> S
        end,
        Stream ! {self(), appendEvents, [1], -1},
        Result = receive
          R -> R
        end,
        ?_assertEqual(ack, Result)
      end
      ]
    end
  }.

appended_event_should_be_saved_to_store_test_() -> {
    setup,
    fun main:bootstrap/0,
    fun main:shutdown/1,
    fun ({Store, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        Stream = receive
          {ok, S} -> S
        end,
        Stream ! {self(), appendEvents, [1], -1},
        receive
          ack -> ok
        end,
        Store ! {self(), load, "test"},
        receive
          Events -> ?_assertEqual([1], Events)
        end
      end
      ]
    end
  }.

observer_should_be_notified_test_() -> {
    setup,
    fun main:bootstrap/0,
    fun main:shutdown/1,
    fun ({_, Registry}) -> [
      begin
        Registry ! {self(), getStream, "test"},
        Stream = receive
          {ok, S} -> S
        end,
        Stream ! {self(), observe},
        Stream ! {self(), appendEvents, [1], -1},
        receive
          ack -> ok
        end,
        receive
          Events -> ?_assertEqual([1], Events)
        end
      end
      ]
    end
  }.
