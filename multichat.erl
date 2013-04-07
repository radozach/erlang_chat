-module(multichat).
-behaviour(gen_server).
-export([start/0,
		login/2,
		whereIsNick/2,
		whereIsRoom/2,
		rooms/1,
		stop/1
		
%		logout/1,
%		roomlist/0,
%		newroom/1,	
%		enter/1,
%		exit/0,	
%		msg/2,
%		send/1
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(user, {nick, pid}).
-record(room, {name, pid}).
-record(server,  {users=[], rooms=[]}).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").

%%% Client API
start() -> 
	gen_server:start_link(?MODULE, [], []).

login(Pid, Nick) ->
	gen_server:call(Pid, {login, Nick}).
	
whereIsNick(Pid, Nick) ->
	gen_server:call(Pid, {wherenick, Nick}).

whereIsRoom(Pid, Room) ->
	gen_server:call(Pid, {whereroom, Room}).
	
rooms(Pid) ->
	gen_server:call(Pid, rooms).
 
stop(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, make_server([],[])}. %% no treatment of info here!
 
handle_call({login, Nick}, _From, Server) ->
	% TODO: kontrola
	Users = Server#server.users, 
	NewUsers = [make_user(Nick,Nick)|Users],
	NewServer = make_server(NewUsers,Server#server.rooms), 
	io:format("LOGGED ~n"),
	{reply, NewServer, NewServer};
	
handle_call(terminate, _From, Server) ->
	{stop, normal, ok, Server}.	

handle_cast(_, Server) ->
	io:format("empty cast!~n",[]),
	{noreply, Server}.

handle_info(Msg, Server) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Server}.

terminate(normal, Server) ->
	io:format("server is off!~n",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_user(Nick, Pid) ->
	#user{nick=Nick, pid=Pid}.
make_room(Name, Pid) ->
	#room{name=Name, pid=Pid}.
make_server(Users, Rooms) ->
	#server{users=Users, rooms=Rooms}.
