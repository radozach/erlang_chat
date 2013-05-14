-module(multichat).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").


-record(user, {nick, pid}).
-record(room, {name, pid}).
-record(worker, {limit=2, pid}).
-record(server,  {users=[], rooms=[], workers=[], backup}).


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
init([]) -> {ok, make_server([],[],[])}. %% no treatment of info here!
 
handle_call({login, Nick}, _From, Server) ->
	% create worker for user
	WorkerPid = find_worker(Server#server.workers,Server),
	if WorkerPid =:= false ->
		   		%worker not found
		   		MyPid = self(), 
				{ok,NewWorker} = gen_server:start_link(multichatworker, [MyPid], []),
				Workers = [make_worker(2,NewWorker)|Server#server.workers],
				NewServer = make_server(Server#server.users,Server#server.rooms,Workers),
				io:format("new worker ~n");
			true ->
				%worker ok
				NewServer = Server,
				NewWorker = WorkerPid
	end,
	NewUsers = [make_user(Nick,NewWorker)|NewServer#server.users],
	NewServer2 = make_server(NewUsers,NewServer#server.rooms,NewServer#server.workers),
	io:format("MAIN LOGGED ~n"),
	{reply, NewWorker, NewServer2};
handle_call(rooms, _From, Server) ->
	AllRooms = [io:format("Room '~p' (on ~p)~n",[R#room.name, R#room.pid]) || R <- Server#server.rooms],
	{reply, AllRooms, Server};
handle_call({newroomonpid, Name, OnPid}, _From, Server) ->
	io:format("Registering~n"),
	NewServer = make_server(Server#server.users,[make_room(Name, OnPid)|Server#server.rooms],Server#server.workers), 
	io:format("New room registered~n"),
	{reply, NewServer, NewServer};
handle_call({whereroom, RoomName}, _From, Server) ->
	OnPid = getRoomPid(RoomName, Server#server.rooms),
	io:format("room '~p' is on ~p~n",[RoomName, OnPid]), 
	{reply, OnPid, Server};
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
	ok;
terminate(_, _) ->
  	io:format("MASTER Unexpected shutdown!"),
	% WAKE BACKUP
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% Private functions
make_user(Nick, Pid) ->
	#user{nick=Nick, pid=Pid}.
	
make_room(Name, Pid) ->
	#room{name=Name, pid=Pid}.
	
make_server(Users, Rooms,Workers) ->
	#server{users=Users, rooms=Rooms, workers=Workers}.
	
make_worker(Limit,Pid) ->
	#worker{limit=Limit,pid=Pid}.

check_workerCreated({ok,WorkerPid}) ->
	WorkerPid;
check_workerCreated(_) ->
	false.


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
  
find_worker([],_) ->
	false;
find_worker([H|T],S)->
	C = users_on_worker(S#server.users,H#worker.pid,0),
	if H#worker.limit > C ->
			H#worker.pid;
		true -> 
			find_worker(T,S)
	end.
	
users_on_worker([],_,C) ->
	C;
users_on_worker([U|T],WP,C) ->
	if U#user.pid =:= WP -> users_on_worker(T,WP,C+1);
		true			 -> users_on_worker(T,WP,C)
	end.

