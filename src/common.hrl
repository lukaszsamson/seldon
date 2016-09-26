-type event() :: any().

-type stream_id() :: iolist().

-type store() :: pid().

-type stream_ctor() :: fun((stream_id(), list(event()), store()) -> pid()).

-type observer() :: pid().
