-module(matmul).

-export([prepare/0, bench/1, bench/2, test/1, mul/3]).

-define(SUBSIZE, 4).
-define(ROWS, 96).
-define(COLS, 96).

prepare() ->
    Pregen = {matgen(), matgen()},
    {Pregen, [?MODULE, lists, epiphany, epiphany_server]}.

bench(Pregen) -> bench(Pregen, 16).
bench({A, B}, NoWorkers) ->
    mul(A, B, NoWorkers).

-type matrix(Of) :: [[Of]].
-type matrix() :: matrix(integer()).

test(N) ->
    {A, B} = {matgen(), matgen()},
    {Time, Res} = timer:tc(fun()-> mul(A, B, N) end),
    case mul_seq(A, B) of
	Res -> Time;
	Correct -> {badmatch, Res, Correct}
    end.

%% @doc Multiply matrices A and B using N workers
mul(A, B, N) ->
    Workers = fork(N),
    try
	As = split_rows(A, []),
	Bs = split_cols(B, []),
	do_mul(As, As, Bs, Workers, Workers, [[]])
    after join(Workers)
    end.

-spec do_mul([matrix()], [matrix()], [matrix()], [pid()], [pid()],
	     matrix(reference())) -> matrix().
do_mul([], _AllAs, [_], _Workers, _AllWorkers, Acc) -> collect(Acc, [[]]);
do_mul([], AllAs, [_B|Bs], Workers, AllWorkers, Acc) ->
    do_mul(AllAs, AllAs, Bs, Workers, AllWorkers, [[]|Acc]);
do_mul(As, AllAs, Bs, [], AllWorkers, Acc) ->
    do_mul(As, AllAs, Bs, AllWorkers, AllWorkers, Acc);
do_mul([A|As], AllAs, [B|_]=Bs, [Worker|Workers], AllWorkers, [Acc|Accs]) ->
    Ref = make_ref(),
    Worker ! {mul, self(), Ref, A, B},
    do_mul(As, AllAs, Bs, Workers, AllWorkers, [[Ref|Acc]|Accs]).

-spec collect(matrix(reference()), matrix(matrix())) -> matrix().
collect([[]], Acc) -> merge(Acc);
collect([[]|Col], Acc) -> collect(Col, [[]|Acc]);
collect([[Ref|Row]|Col], [A|Acc]) ->
    receive {Ref, Mat} ->
            collect([Row|Col], [[Mat|A]|Acc])
    end.

%% @doc Flattens a MxN matrix of KxK matrices into a MKxNK matrix.
-spec merge(matrix(matrix())) -> matrix().
merge([[]|_]) -> [];
merge(Mat) ->
    Heads = [hd(Row) || Row <- Mat],
    Tails = [tl(Row) || Row <- Mat],
    merge_row(Heads, []) ++ merge(Tails).

-spec merge_row([matrix()], matrix()) -> matrix().
merge_row([[]|_], Acc) -> lists:reverse(Acc);
merge_row(Row, Acc) ->
    Heads = [hd(Mat) || Mat <- Row],
    Tails = [tl(Mat) || Mat <- Row],
    MergedRow = lists:append(Heads),
    merge_row(Tails, [MergedRow | Acc]).

-spec fork(integer()) -> [pid()].
fork(N) ->
    {Spawn, _Count} = colib:spawn_and_count(),
    [Spawn(fun worker_loop/0) || _ <- lists:seq(1, N)].

join(Workers) ->
    Refs = [monitor(process, W) || W <- Workers],
    lists:foreach(fun(W) -> W ! stop end, Workers),
    lists:foreach(fun(R) -> receive {'DOWN', R, _, _, _} -> ok end end, Refs).

worker_loop() ->
    receive
	stop -> ok;
	{mul, From, Ref, A, B} ->
	    From ! {Ref, mul_seq(A, B)},
	    worker_loop()
    end.

%% Multiplies two matrices
-spec mul_seq(matrix(), matrix()) -> matrix().
mul_seq(ARows, B) ->
    BCols = transpose(B),
    [[mul_strips(ARow, BCol) || BCol <- BCols]
     || ARow <- ARows].

mul_strips(A, B) -> mul_strips(A, B, 0).
mul_strips([], [], Acc) -> Acc;
mul_strips([A|As], [B|Bs], Acc) ->
    mul_strips(As, Bs, A*B + Acc).

-spec transpose(matrix()) -> matrix().
transpose([[]|_]) -> [];
transpose(Mat) ->
    Heads = [hd(Row) || Row <- Mat],
    Tails = [tl(Row) || Row <- Mat],
    [Heads | transpose(Tails)].

-spec matgen() -> matrix().
matgen() ->
    random:seed(now()),
    [[random:uniform(3) || _Col <- lists:seq(1, ?COLS)]
     || _Row <- lists:seq(1, ?ROWS)].

%% @doc Cuts a (M*?SUBSIZE)xN matrix into M ?SUBSIZExN matrices.
-spec split_rows(matrix(), [matrix()]) -> [matrix()].
split_rows([], Acc) -> lists:reverse(Acc);
split_rows(Mat, Acc) ->
    {Row, Rest} = lists:split(?SUBSIZE, Mat),
    split_rows(Rest, [Row|Acc]).

%% @doc Cuts a Mx(N*?SUBSIZE) matrix into N Mx?SUBSIZE matrices.
-spec split_cols(matrix(), [matrix()]) -> [matrix()].
split_cols([[]|_], Acc) -> lists:reverse(Acc);
split_cols(Mat, Acc) ->
    Tuples = [lists:split(?SUBSIZE, Row) || Row <- Mat],
    Col  = [Row ||  {Row, _Rest} <- Tuples],
    Rest = [Rest || {_Row, Rest} <- Tuples],
    split_cols(Rest, [Col|Acc]).
