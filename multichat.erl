-module(multichat).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-record(user, {nick, pid}).
-record(room, {name, pid}).
-record(server,  {users=[], rooms=[]}).
-export([start/0, 
 login/2,
%		whereIsNick/2, whereIsRoom/2,
%		these fnctions cant be part of api - only worker will call request!
		users/1, rooms/1,
		stop/1
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Client API
start() ->
	%gen_server:start_link(?MODULE, [], []). 
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(multichatapp, Pid),
	io:format("'multichatapp' is registered now! ~n"),
	{ok, Pid}.

login(Pid, Nick) ->
	gen_server:call(Pid, {login, Nick}).
	
%whereIsNick(Pid, Nick) ->
%	gen_server:call(Pid, {wherenick, Nick}).

%whereIsRoom(Pid, Room) ->
%	gen_server:call(Pid, {whereroom, Room}).
	
rooms(Pid) ->
	gen_server:call(Pid, rooms).
	
users(Pid) ->
	gen_server:call(Pid, users).
 
stop(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, make_server([],[])}. %% no treatment of info here!
 
handle_call({login, Nick}, _From, Server) ->
	% create worker for user
	MyPid = self(), 
	{ok, WorkerPid} = gen_server:start_link(multichatworker, [MyPid], []),
	NewUsers = [make_user(Nick,WorkerPid)|Server#server.users],
	NewServer = make_server(NewUsers,Server#server.rooms),
	io:format("LOGGED ~n"),
	{reply, NewServer, NewServer};
handle_call(rooms, _From, Server) ->
	[io:format("Room '~p' (on ~p)~n",[R#room.name, R#room.pid]) || R <- Server#server.rooms],
	{reply, Server, Server};
handle_call({newroomonpid, Name, OnPid}, _From, Server) ->
	NewServer = make_server(Server#server.users,[make_room(Name, OnPid)|Server#server.rooms]), 
	io:format("New room registered~n"),
	{reply, NewServer, NewServer};
handle_call({whereroom, RoomName}, _From, Server) ->
	OnPid = getRoomPid(RoomName, Server#server.rooms),
	io:format("room '~p' is on ~p~n",[RoomName, OnPid]), 
	{reply, Server, Server};
handle_call({wherenick, Nick}, _From, Server) ->
	OnPid = getUserPid(Nick, Server#server.users),
	io:format("user '~p' is on ~p~n",[Nick, OnPid]), 
	{reply, Server, Server};
handle_call(users, _From, Server) ->
	[io:format("User '~p' (on ~p)~n",[U#user.nick, U#user.pid]) || U <- Server#server.users],
	{reply, Server, Server};
handle_call(terminate, _From, Server) ->
	{stop, normal, ok, Server}.	

handle_cast(_, Server) ->
	io:format("empty cast!~n",[]),
	{noreply, Server}.

handle_info(Msg, Server) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Server}.

terminate(normal, _) ->
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

% obsahuje prvok
getUserPid(_, []) ->
	false;
getUserPid(Nick, [User|_]) when User#user.nick =:= Nick ->
	User#user.pid;
getUserPid(Nick, [_|T]) ->
	getUserPid(Nick, T).
  
getRoomPid(_, []) ->
	false;
getRoomPid(Name, [Room|_]) when Room#room.name =:= Name ->
  Room#room.pid;
getRoomPid(Name, [_|T]) ->
  getRoomPid(Name, T).
