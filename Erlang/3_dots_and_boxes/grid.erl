% Name: Francesco Pavlovic
% UvAnetID: 13782118
% Study: B.S.c Informatica

% This document allows the program to manipulate a grid structure needed for a
% game of dots and boxes. This grid structure is maintained using a tuple
% containing width, height and a list containing all walls, which are noted by
% using a tuple containing two other tuples that hold the adjacent node
% coordinates. This document can calculate the score that a player has gotten
% in a single move, detect spots that are open, and find walls that can be
% completed.

-module(grid).
-export([new/2, get_wall/3, has_wall/2, add_wall/2, show_hlines/2,
show_vlines/2, print/1, get_cell_walls/2, node_completed/3, get_all_walls/2,
get_open_spots/1, choose_random_wall/1, score_gained/2, grid_empty/1,
get_open_cell_walls/3, get_completable_walls/1, choose_completable_wall/1]).

% Creates a new grid.
new(Width, Height) ->
    {Width, Height, []}.

% Retrieves the wall that is in direction Dir of Node(X, Y).
get_wall(X, Y, Dir) when Dir == north -> {{X,Y-1},{X,Y}};
get_wall(X, Y, Dir) when Dir == south -> {{X,Y},{X,Y+1}};
get_wall(X, Y, Dir) when Dir == west -> {{X-1,Y},{X,Y}};
get_wall(X, Y, Dir) when Dir == east -> {{X,Y},{X+1,Y}}.

% True if Wall is in the Grid, else False.
has_wall(Wall, Grid) ->
    lists:member(Wall, element(3, Grid)).

% Returns a grid with Wall added to Grid.
add_wall(Wall, Grid) ->
    case has_wall(Wall, Grid) of
    true -> Grid;
    false -> {X, Y, CurrentWalls} = Grid,
            NewWalls = lists:append(CurrentWalls, [Wall]),
            {X, Y, NewWalls}
    end.

% Helper function that prints all node intersections and horizontal walls
% placed at height Row.
show_hlines(Row, Grid) ->
    HLineString = [String || X <- lists:seq(0, element(1, Grid) - 1),
                                String <- case has_wall(get_wall(X, Row, north), Grid) of
                                            true -> "+--";
                                            false -> "+  "
                                            end],
    HLineString ++ "+~n".

% Helper function that prints all vertical walls placed at height Row.
show_vlines(Row, Grid) ->
    VLineString = [String || X <- lists:seq(0, element(1, Grid) - 1),
                                String <- case has_wall(get_wall(X, Row, west), Grid) of
                                true -> "|  ";
                                false -> "   "
                                end],

    LastStripe = case has_wall(get_wall(element(1, Grid), Row, west), Grid) of
        true -> "|~n";
        false -> " ~n"
    end,

    VLineString ++ LastStripe.

% Retrieves a list that gets walls from every direction from Node(X, Y).
get_cell_walls(X, Y) ->
    N = get_wall(X, Y, north),
    S = get_wall(X, Y, south),
    W = get_wall(X, Y, west),
    E = get_wall(X, Y, east),

    [N, E, S, W].

% Helper function that gets every wall that can be placed from Grid(X, Y).
get_open_cell_walls(X, Y, Grid) ->
    CellWalls = get_cell_walls(X, Y),
    OpenWalls = CellWalls -- element(3, Grid),
    lists:reverse(lists:sort(OpenWalls)).

% Gets a list that contains walls that can complete a node.
get_completable_walls(Grid) ->
    {W, H, _} = Grid,
    [OpenWalls || X <- lists:seq(0, W-1), Y <- lists:seq(0, H-1),
                    OpenWalls <- get_open_cell_walls(X, Y, Grid),
                    length(get_open_cell_walls(X, Y, Grid)) == 1].

% True if all cell walls are in Grid, otherwise False.
node_completed(X, Y, _Grid) when (X < 0) orelse (Y < 0) ->
    false;

node_completed(X, Y, Grid) ->
    [N, E, S, W] = get_cell_walls(X, Y),
    BoolList = [has_wall(N, Grid), has_wall(S, Grid), has_wall(W, Grid),
    has_wall(E, Grid)],
    lists:all(fun(Bool) -> Bool end, BoolList).

% Calculates the score that a move will gain the player.
score_gained(Wall, Grid) ->
    {XA, YA} = element(1, Wall),
    {XB, YB} = element(2, Wall),

    Nodes = [node_completed(XA, YA, Grid), node_completed(XB, YB, Grid)],
    case lists:all(fun(Bool) -> Bool end, Nodes) of
        true -> 2;
        false -> case lists:member(true, Nodes) of
            true -> 1;
            false -> 0
        end
    end.

% Gets all walls in a grid of the width W by height H.
get_all_walls(W, H) ->
    AllWallsList = [Walls || X <- lists:seq(0, W-1),
                                Y <- lists:seq(0, H-1),
                                Walls <- lists:flatten(get_cell_walls(X, Y))],
    lists:usort(AllWallsList).

% Gets all spots in Grid where a wall can be placed.
get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

% Helper function that checks if a grid is empty.
grid_empty(Grid) ->
    EmptySpots = get_open_spots(Grid),
    if EmptySpots == [] ->
        true;
        true -> false
    end.

% Randomly selects a wall to be placed.
choose_random_wall(Grid) ->
    case length(get_open_spots(Grid)) == 0 of
        true -> [];
        false -> Random = rand:uniform(length(get_open_spots(Grid))),
                 lists:nth(Random, get_open_spots(Grid))
    end.

% Randomly selects a wall that will complete a node to be placed.
choose_completable_wall(Grid) ->
    case length(get_completable_walls(Grid)) == 0 of
        true -> choose_random_wall(Grid);
        false -> Random = rand:uniform(length(get_completable_walls(Grid))),
                 lists:nth(Random, get_completable_walls(Grid))
    end.


% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
    {_, H, _} = Grid,
    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),

        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),
    io:fwrite("~n"),
    ok.
