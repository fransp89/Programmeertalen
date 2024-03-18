-module(grid).
-export([new/2, get_wall/3, has_wall/2, add_wall/2, show_hlines/2,
show_vlines/2, print/1, get_cell_walls/2, get_all_walls/2, get_open_spots/1,
choose_random_wall/1]).
%

% TODO: The other functions.
new(Width, Height) ->
    {Width, Height, []}.


get_wall(X, Y, Dir) when Dir == north -> {{X,Y-1},{X,Y}};
get_wall(X, Y, Dir) when Dir == south -> {{X,Y},{X,Y+1}};
get_wall(X, Y, Dir) when Dir == west -> {{X-1,Y},{X,Y}};
get_wall(X, Y, Dir) when Dir == east -> {{X,Y},{X+1,Y}}.


has_wall(Wall, Grid) ->
    lists:member(Wall, element(3, Grid)).

add_wall(Wall, Grid) ->
    case has_wall(Wall, Grid) of
    true -> Grid;
    false -> {X, Y, CurrentWalls} = Grid,
            NewWalls = CurrentWalls ++ Wall,
            {X, Y, [NewWalls]}
    end.

show_hlines(Row, Grid) ->
    HLineString = [String || X <- lists:seq(0, element(1, Grid) - 1),
                                String <- case has_wall(get_wall(X, Row, north), Grid) of
                                            true -> "+--";
                                            false -> "+  "
                                            end],
    HLineString ++ "+~n".


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

get_cell_walls(X, Y) ->
    NWall = get_wall(X, Y, north),
    SWall = get_wall(X, Y, south),
    WWall = get_wall(X, Y, west),
    EWall = get_wall(X, Y, east),

    [NWall, SWall, WWall, EWall].

get_all_walls(W, H) ->
    AllWallsList = [Walls || X <- lists:seq(0, W-1),
                                Y <- lists:seq(0, H-1),
                                Walls <- lists:flatten(get_cell_walls(X, Y))],
    lists:usort(AllWallsList).

get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

choose_random_wall(Grid) ->
    case length(get_open_spots(Grid)) == 0 of
        true -> [];
        false -> Random = rand:uniform(length(get_open_spots(Grid))),
                    lists:nth(Random, get_open_spots(Grid))
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
