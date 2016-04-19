-module(opaque).

-export([m0/0, s0/0, sm0/0]).

-export_type([t/0, m/0, s/1, sm/1]).

-opaque t()   :: #{atom() => integer()}.

-opaque m()   :: #{t() => integer()}.

-opaque s(K)  :: #{K => integer(), integer() => atom()}.

-opaque sm(K) :: #{K := integer(), integer() := atom()}.

-spec m0() -> m().
m0() ->
    #{#{} => 3}.

-spec s0() -> s(atom()).
s0() -> #{3 => a}.

-spec sm0() -> sm(1).
sm0() -> #{1 => 2, 3 => a}.
