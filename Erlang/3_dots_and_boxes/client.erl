-module(client).
-import(grid, [choose_random_wall/1]).
-export([move/0, new/0]).


move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024,{S1, S2, S3}),
    receive
        finished ->
            io:format("~p: I am done~n", [self()]);
        {move, ServerPid, Grid} ->
            ServerPid ! {move, choose_random_wall(Grid)}
    end.


new() ->
    spawn(client, move, []).
