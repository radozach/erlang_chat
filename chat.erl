-module(chat).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-export([create/0, die/1,
		login/1,
		enter/2, exit/0,	
		msg/2
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Client API
create() -> 
	gen_server:start_link(?MODULE, [], []).

login(Nick) -> 
	gen_server:call(multichatapp, {login, Nick}).

enter(Pid, RoomName) ->
	gen_server:call(Pid, {enter, RoomName}).
	
exit() ->
	todo.

msg(To, Msg) ->
	todo.
 
die(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> 
	{ok, []}. 
 
handle_call({enter, RoomName}, _From, Client) ->
	io:format("you are now in '~p' room!~n",[RoomName]),
	{reply, Client, Client};
handle_call(terminate, _From, Client) ->
	{stop, normal, ok, Client}.	

handle_cast({take}, Client) ->
	io:format("This elevator is empty now!~n",[]),
	{noreply, Client}.

handle_info(Msg, Client) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Client}.

terminate(normal, _) ->
	io:format("client server is hutting down...!~n",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions

    