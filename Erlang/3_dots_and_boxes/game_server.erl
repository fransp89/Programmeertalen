-module(game_server).

-behaviour(gen_server).

-import(grid, [get_open_spots/1, has_wall/2, add_wall/2, node_completed/3,
decide_turn_order/2, score_gained/2, grid_empty/1]).
-export([start_link/1, handle_call/3, handle_cast/2, decide_turn_order/3,
flip_turn_order/1]).
-export([init/1, move/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).


% TODO: You need to inform the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [P1 | _P2] = Players,
    P1 ! {move, self(), Grid},
    {ok, {Grid, Players}}.


is_wall_legal(Wall, Grid) ->
    WallLegality = lists:member(Wall, get_open_spots(Grid)),
    case WallLegality of
        true ->
            add_wall(Wall, Grid);
        false ->
            Grid
    end.

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

% TODO: add handle_call for move.
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
