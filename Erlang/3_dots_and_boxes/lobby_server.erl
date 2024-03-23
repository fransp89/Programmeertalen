-module(lobby_server).

-behaviour(gen_server).

-export([start/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, new_game/3, games/0]).

% You need to implement the call for this to work.
% Should respond with {ok, PidOfCreatedGame}
new_game(W, H, Players) ->
    gen_server:call(lobby_server, {new_game, W, H, Players}).

games() ->
    gen_server:call(lobby_server, games).

% Call this when you want to start the lobby server.
start() ->
    gen_server:start({local, lobby_server}, lobby_server, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

% TODO: add handle_call to make new_game/3 work.
handle_call({new_game, W, H, Players}, _From, Games) ->
    {ok, GamePid} = game_server:start_link({W, H, Players}),
    {reply, {ok, GamePid}, [GamePid | Games]};

handle_call(games, _From, Games) ->
    {reply, Games, Games}.

% Required for gen_server behaviour.
% Normally you would implement this to,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.

handle_info({'EXIT', From, normal}, State) ->
    {noreply, lists:delete(From, State)};

handle_info({'EXIT', From, shutdown}, State) ->
    {noreply, lists:delete(From, State)}.
