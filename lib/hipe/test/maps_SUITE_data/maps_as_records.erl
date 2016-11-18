-module(maps_as_records).
-export([test/0]).

test() ->
    #{last := 2, current := 3} = fib_step(#{last => 1, current => 2}),
    #{last := 3, current := 5} = fib_step(#{last => 2, current => 3}),
    #{type := result, value := 8} = sum(3, 5),
    #{type := error, value := badarith} = sum(q, 1.0),
    ok.

fib_step(#{last := Last, current := Current}) ->
    #{last => Current, current => Last + Current}.

sum(A, B) ->
    try A + B
    of Result -> #{type => result, value => Result}
    catch error:E -> #{type => error, value => E}
    end.
