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

-record(room, {name, pid}).
-record(client, {nick, worker, room=false}).

%%% Client API
create() -> 
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	io:format("CLIENT: server started!"),
	Pid.

login(Pid, Nick) -> 
	gen_server:call(Pid,{login, Nick}).
	
rooms(Pid) ->
	gen_server:call(multichatapp, rooms).
	%[io:format("Room '~p' (on ~p)~n", [R#room.name, R#room.pid]) || R <- Rooms].

make_room(Pid, RoomName) ->
	gen_server:call(Pid, {new_room, RoomName}).

enter(Pid, RoomName) ->
	gen_server:call(Pid, {enter, RoomName}).
	
users(Pid) ->
	gen_server:call(multichatapp, users).

exit() ->
	todo.

send(Pid,Msg) ->
	gen_server:call(Pid, {room_msg,Msg}).

msg(Pid, To, Msg) ->
	todo.
 
die(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, make_client()}. 

handle_call({login, Nick},_From,Client) ->
	Resp = gen_server:call(multichatapp, {login, Nick}),
	Logged = check_login(Resp),
	if Logged =:= true ->
		   %save worker pid
			gen_server:cast(self(), {save_worker, Nick, Resp}),
			io:format("CLIENT: I'm logged:)~n");
	   true ->
		   io:format("CLIENT: Loging failed:(")
	end,
	{reply,ok,Client};

handle_call({new_room, RoomName}, _From, Client) ->
	io:format("CLIENT: Creating room ~p~n",[RoomName]),
	Resp = gen_server:call(Client#client.worker, {new_room, RoomName}),
	{reply, Resp, Client}; 
	
handle_call({enter, RoomName}, _From, Client) ->
	Resp = gen_server:call(Client#client.worker, {enter_room, RoomName, Client#client.nick}),
	if Resp =:= false ->
			{reply, Resp, make_client(	Client#client.nick, 
										Client#client.worker, 
										Resp)};
		true ->
			{reply, Resp, make_client(	Client#client.nick, 
										Client#client.worker, 
										make_room_rec(RoomName, Resp))}
	end;		
								
handle_call({room_msg, Msg}, _From, Client) ->
	if Client#client.room =:= false ->
			io:format("Not in room!~n");
		true ->
			R = Client#client.room,
			gen_server:cast(R#room.pid, {room_msg,Client#client.nick,R#room.name,Msg})
	end,
	{reply, ok, Client};

handle_call(terminate, _From, Client) ->
	{stop, normal, ok, Client}.	

handle_cast({save_worker, Nick, WorkerPid}, _Client) ->
	io:format("CLIENT: Worker assigned!~n"),
	{noreply, make_client(Nick, WorkerPid, false)}.

handle_info(Msg, Client) ->
	io:format("CLIENT: Unexpected message: ~p~n",[Msg]),
	{noreply, Client}.
terminate(normal, _) ->
	io:format("CLIENT: terminate~n"),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_client() ->
	#client{nick=empty, worker=empty, room=false}.
make_client(Nick, WorkerPid, Room) ->
	#client{nick=Nick, worker=WorkerPid, room=Room}.
make_room_rec(Name,Pid) ->
	#room{name=Name, pid=Pid}.

check_login(false) ->
	false;
check_login(_) ->
	true.



    