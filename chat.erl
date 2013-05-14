-module(chat).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-export([create/0, die/1,
		login/2,
		rooms/1,
		users/1,
		enter/2, exit/0, make_room/2,	
		msg/3, send/2
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(client, {nick, worker, room_pid}).




%%% Client API
create() -> 
	Resp = gen_server:start_link(?MODULE, [], []),
	{ok,Pid} = Resp,
	Pid.

login(Pid, Nick) -> 
	Resp = gen_server:call(multichatapp, {login, Nick}),
	Logged = check_login(Resp),
	if Logged =:= true ->
		   %save worker pid
			gen_server:cast(Pid, {save_worker, Nick, Resp}),
			io:format("Client logged.~n",[]);
	   true ->
		   io:format("Loging failed.")
	end.
	
rooms(Pid) ->
	gen_server:call(multichatapp, rooms).
	%[io:format("Room '~p' (on ~p)~n",[R#room.name, R#room.pid]) || R <- Rooms].

make_room(Pid, Name) ->
	gen_server:call(Pid,{make_room, Name}).

enter(Pid, RoomName) ->
	gen_server:call(Pid, {enter, RoomName}).
	
users(Pid) ->
	gen_server:call(multichatapp, users).

exit() ->
	todo.

send(Pid,Msg) ->
	todo.

msg(Pid, To, Msg) ->
	todo.
 
die(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> 
	{ok, make_client()}. 

handle_call({make_room, RoomName}, _From, Client) ->
	io:format("Creating room ~p~n",[RoomName]),
	Resp = gen_server:call(Client#client.worker, {make_room,RoomName}),
	{reply, Resp, Client}; 
handle_call({enter, RoomName}, _From, Client) ->
	Resp = gen_server:call(Client#client.worker, {enter_room,RoomName,Client#client.nick}),
	NewClient = make_client(Client#client.nick,Client#client.worker,Resp),
	{reply, Resp, NewClient}; 
handle_call(terminate, _From, Client) ->
	{stop, normal, ok, Client}.	

handle_cast({save_worker, Nick, WorkerPid}, _Client) ->
	NewClient = make_client(Nick,WorkerPid,0),
	io:format("Worker assigned!~n",[]),
	{noreply, NewClient}.

handle_info(Msg, Client) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Client}.

terminate(normal, _) ->
	io:format("client server is hutting down...!~n",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_client() ->
	#client{nick=empty, worker=empty, room_pid=0}.
make_client(Nick, WorkerPid, RoomPid) ->
	#client{nick=Nick, worker=WorkerPid, room_pid=RoomPid}.

check_login(false) ->
	false;
check_login(_) ->
	true.



    