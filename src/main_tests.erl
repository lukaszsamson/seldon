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
