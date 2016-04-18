-module(opaque_key).

-export([m0/0]).

-export_type([t/0, m/0]).

-opaque t() :: #{atom() => integer()}.

-opaque m() :: #{t() => integer()}.

-spec m0() -> m().

m0() ->
    #{#{} => 3}.
