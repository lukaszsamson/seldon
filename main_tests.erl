-module(main_tests).
-include_lib("eunit/include/eunit.hrl").

bootstrap_should_work_test() ->
  main:bootstrap().
