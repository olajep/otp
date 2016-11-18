-module(maps_as_records).
-export([test/0]).

test() ->
    #{last := 2, current := 3} = fib_step(#{last => 1, current => 2}),
    #{last := 3, current := 5} = fib_step(#{last => 2, current => 3}),
    ok.

fib_step(#{last := Last, current := Current}) ->
    #{last => Current, current => Last + Current}.
