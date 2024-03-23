% Name: Francesco Pavlovic
% UvAnetID: 13782118
% Study: B.S.c Informatica

% A client that is able to receive messages about the state of the game, and
% to send back moves the player wants to make.

-module(client).
-import(grid, [choose_completable_wall/1]).
-export([move/0, new/0]).

% Send a message {move, Wall} to the game server.
move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024,{S1, S2, S3}),
    receive
        finished ->
            io:format("~p: I am done~n", [self()]);
        {move, ServerPid, Grid} ->
            gen_server:call(ServerPid, {move, choose_completable_wall(Grid)}),
            move()
    end.

% Creates a new client.
new() ->
    spawn(client, move, []).
