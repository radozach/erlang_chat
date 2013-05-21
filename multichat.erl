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
%		whereIsNick/2, whereIsRoom/2,  % not in api - only worker will call request!
		users/1, rooms/1,
		stop/1
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Client API
start() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(multichatapp, Pid), 
	io:format("'multichatapp' is registered now! ~n"),
	Ref = erlang:monitor(process, multichatapp),
	io:format("'multichatapp' is monitored now ~p! ~n",[Ref]),
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

  
handle_call({login, Nick}, From, Server) ->
	MyPid = logged(Server#server.users, Nick),
	if MyPid =:= false ->
			% i am not logged already
			WorkerPid = find_worker(Server#server.workers, Server),
			if WorkerPid =:= false ->
					% we need new worker
					{ok, NewWorker} = gen_server:start_link(multichatworker, [self()], []),
					NewServer = make_server([make_user(Nick, NewWorker)|Server#server.users],
											Server#server.rooms, 
											[make_worker(2, NewWorker)|Server#server.workers]),
					io:format("MASTER: New worker created!~n");
				true ->
					NewServer = make_server([make_user(Nick, WorkerPid)|Server#server.users],
											Server#server.rooms, 
											Server#server.workers),
					NewWorker = WorkerPid
			end,
			io:format("MASTER: User ~p logged! ~n", [Nick]);
		true ->
			io:format("MASTER: I am already logged!~n"),
			NewWorker = MyPid,
			NewServer = Server
	end,
	
	{Pid, _} = From,
	gen_server:cast(NewWorker, {register_new_client, Pid, Nick}),
	
	{reply, NewWorker, NewServer};
	
handle_call(users, _From, Server) ->
	AllUsers = [io:format("User '~p' (on ~p)~n",[U#user.nick, U#user.pid]) || U <- Server#server.users],
	{reply, AllUsers, Server};
handle_call(rooms, _From, Server) ->
	AllRooms = [io:format("Room '~p' (on ~p)~n",[R#room.name, R#room.pid]) || R <- Server#server.rooms],
	{reply, AllRooms, Server};

handle_call({newroomonpid, Name, OnPid}, _From, Server) ->
	io:format("MASTER: New room (~p) registered~n", [Name]),
	{reply, ok, make_server(	Server#server.users, 
								[make_room(Name, OnPid)|Server#server.rooms],
								Server#server.workers)};

handle_call({whereroom, RoomName}, _From, Server) ->
	OnPid = getRoomPid(RoomName, Server#server.rooms),
	io:format("MASTER: Room '~p' is on ~p~n",[RoomName, OnPid]), 
	{reply, OnPid, Server};
handle_call({wherenick, Nick}, _From, Server) ->
	OnPid = getUserPid(Nick, Server#server.users),
	io:format("MASTER: User '~p' is on ~p~n",[Nick, OnPid]), 
	{reply, OnPid, Server};

handle_call(terminate, _From, Server) ->
	{stop, normal, ok, Server}.	

handle_cast({'DOWN', R, _, _, _}, Server) ->
	io:format("MASTER: DOWN message...!~n"),
	start(),
	{noreply, Server};
handle_cast(_, Server) ->
	io:format("MASTER: empty cast!~n"),
	{noreply, Server}.
handle_info(Msg, Server) ->
	io:format("MASTER: Unexpected message: ~p~n",[Msg]),
	{noreply, Server}.
	
terminate(normal, _) ->
	io:format("MASTER: terminate (normal)~n"),
	ok;
terminate(_, _) ->
  	io:format("MASTER: terminate~n"),
	start(),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_user(Nick, Pid)				 -> #user{nick=Nick, pid=Pid}.
make_room(Name, Pid)				 -> #room{name=Name, pid=Pid}.
make_server(Users, Rooms, Workers)	 -> #server{users=Users, rooms=Rooms, workers=Workers}.
make_worker(Limit,Pid)				 -> #worker{limit=Limit, pid=Pid}.

% obsahuje prvok
getUserPid(_, []) -> false;
getUserPid(Nick, [User|_]) when User#user.nick =:= Nick ->
	User#user.pid;
getUserPid(Nick, [_|T]) ->
	getUserPid(Nick, T).
  
getRoomPid(_, []) -> false;
getRoomPid(Name, [Room|_]) when Room#room.name =:= Name ->
	Room#room.pid;
getRoomPid(Name, [_|T]) ->
	getRoomPid(Name, T).
  
logged([],_) ->
	false;
logged([H|T], UserNick) when H#user.nick =:= UserNick ->
	H#user.pid;
logged([_|T], UserNick) ->
	logged(T, UserNick).	

find_worker([], _) -> false;
find_worker([H|T], S)->
	C = users_on_worker(S#server.users, H#worker.pid, 0),
	if H#worker.limit > C	 -> H#worker.pid;
		true				 -> find_worker(T, S)
	end.
	
users_on_worker([], _, C) -> C;
users_on_worker([U|T], WP, C) ->
	if U#user.pid =:= WP -> users_on_worker(T, WP, C+1);
		true			 -> users_on_worker(T, WP, C)
	end.

