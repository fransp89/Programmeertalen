% Name: Francesco Pavlovic
% UvAnetID: 13782118
% Study: B.S.c Informatica

% The game server hosts the main processes to run the server on which boxes
% and dots can be played on. It sends updated grids to the players, determines
% order of play, and is able to determine move legality. It's able to send
% {move, ServerPID, Grid} to the client.

-module(game_server).

-behaviour(gen_server).

-import(grid, [get_open_spots/1, add_wall/2, node_completed/3, score_gained/2,
grid_empty/1]).
-export([start_link/1, handle_call/3, handle_cast/2, decide_turn_order/3,
flip_turn_order/1]).
-export([init/1, move/2]).

% Creates new process named game_server.
start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).

% Creates a new grid, chooses a player to start the game, and sends the signal
% to make the first move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [P1 | _P2] = Players,
    P1 ! {move, self(), Grid},
    {ok, {Grid, Players}}.

% Gives a new grid with Wall added onto it, if the spot is open, otherwise
% returns Grid back.
is_wall_legal(Wall, Grid) ->
    WallLegality = lists:member(Wall, get_open_spots(Grid)),
    case WallLegality of
        true ->
            add_wall(Wall, Grid);
        false ->
            Grid
    end.

% Changes the turn order.
flip_turn_order(Players) ->
    lists:reverse(Players).

decide_turn_order(Wall, Grid, Players) ->
    {XA, YA} = element(1, Wall),
    {XB, YB} = element(2, Wall),
    case node_completed(XA, YA, Grid) of
        false -> case node_completed(XB, YB, Grid) of
                    true -> Players;
                    false -> flip_turn_order(Players)
        end;
        true -> Players
    end.

% This function adds Wall to the grid, determines the score of the grid, and
% changes turn owner if necessary. Sends a message back with {ok, Score}.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    NewGrid = is_wall_legal(Wall, Grid),
    case Grid == NewGrid of
        false ->
            Score = score_gained(Wall, NewGrid),
            Empty = grid_empty(NewGrid),
            case Empty of
                true ->
                    lists:map(fun(Client) -> Client ! finished end, Players),
                    NewState = {NewGrid, Players};
                false ->
                    TurnOrder = decide_turn_order(Wall, NewGrid, Players),
                    NewState = {NewGrid, TurnOrder},
                    hd(Players) ! {move, self(), NewGrid}
            end;
        true ->
            TurnOrder = flip_turn_order(Players),
            Score = 0,
            NewState = {NewGrid, TurnOrder},
            hd(Players) ! {move, self(), NewGrid}
    end,
    {reply, {ok, Score}, NewState};

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
