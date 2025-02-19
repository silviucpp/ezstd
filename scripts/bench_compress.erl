-module(bench_compress).
-export([main/1]).

generate_data(Size) ->
    rand:seed({exsss,[244613567848733509|123751949107613598]}),
    binary:copy(rand:bytes(trunc(Size/10)), 10).

main(_) ->
    Data = generate_data(50000000),
    Times = bench(Data, 10),
    io:format("AVG Time: ~p~n", [lists:sum(Times) / length(Times)]),
    erlang:halt().

bench(_Data, 0) ->
    [];
bench(Data, Count) ->
    {Time, _} = timer:tc(ezstd, compress, [Data, 15]),
    io:format("Time: ~p~n", [Time]),
    [Time | bench(Data, Count - 1)].
