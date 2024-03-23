% Name: Francesco Pavlovic
% UvAnetID: 13782118
% Study: B.S.c Informatica

-module(lobby_server).

-behaviour(gen_server).

-export([start/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, new_game/3, games/0]).

% You need to implement the call for this to work.
% Should respond with {ok, PidOfCreatedGame}
new_game(W, H, Players) ->
    gen_server:call(lobby_server, {new_game, W, H, Players}).

% Gives the list of running games back.
games() ->
    gen_server:call(lobby_server, games).

% Call this when you want to start the lobby server.
start() ->
    gen_server:start({local, lobby_server}, lobby_server, [], []).

% Creates the list that contain running games, and allows timeouts to send an
% 'EXIT' message.
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

% Helper function for new_game().
handle_call({new_game, W, H, Players}, _From, Games) ->
    {ok, GamePid} = game_server:start_link({W, H, Players}),
    {reply, {ok, GamePid}, [GamePid | Games]};

% Helper function for games().
handle_call(games, _From, Games) ->
    {reply, Games, Games}.

% Required for gen_server behaviour.
% Normally you would implement this to,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.

% Handles a game stopping.
handle_info({'EXIT', From, normal}, State) ->
    {noreply, lists:delete(From, State)};

handle_info({'EXIT', From, shutdown}, State) ->
    {noreply, lists:delete(From, State)}.
