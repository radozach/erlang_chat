-module(multichatworker).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").

-record(msg, {ufrom, uto, text}).
-record(user, {nick, pid}).
-record(room, {name, pid, users, msgs=[]}).
-record(worker, {mypid, mainpid, backup, users, rooms}).

-export([
%		create/1,     % is called by mainserver
		sendtouser/3,
		die/1	
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Client API
sendtouser(MyPid, ToNick, Msg) ->
	% ToPid = getPid(ToNick), 
	gen_server:call(MyPid, {message, MyPid, ToNick, Msg}).

die(MyPid) ->
	gen_server:call(MyPid, terminate).

%%% Server functions
init(ParentPid) -> {ok, make_worker(self(), ParentPid, false, [], [])}. 
 
% receive message
handle_call({message, FromPid, ToPid, Msg}, _From, Worker) when ToPid =:= Worker#worker.mypid ->
	% todo = zistit nick pidu odkial prisla sprava (opytat sa servera ak nemam ulozeneho)
	% FromNick = getNick(FromPid),
	FromNick = noname,
	io:format("Received msg (from ~p): ~p~n", [FromNick, Msg]),
	{reply, Worker, Worker};
	
handle_call({enter_room, RoomName, UserNick}, _From, Worker) ->
	Room = find_room(Worker#worker.rooms, RoomName),
	if Room =:= false ->
			% tento worker este nekomunikoval s miestnostou 
			RoomPid = gen_server:call(multichatapp, {whereroom, RoomName});
		true ->
			RoomPid = Room#room.pid
	end,
	gen_server:cast(RoomPid, {add_user_to_room, RoomName, make_user(UserNick, self())}),
	{reply, RoomPid, Worker};
							
handle_call({new_room, RoomName}, _From, Worker) ->
	Room = make_room(RoomName, self(), [], []),
	gen_server:call(multichatapp, {newroomonpid, RoomName, Worker#worker.mypid}),
	{reply, ok, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							Worker#worker.users,
							[Room|Worker#worker.rooms])};

handle_call(terminate, _From, Worker) ->
	{stop, normal, ok, Worker}.	

handle_cast({register_new_client, CPid,Nick}, Worker) ->
	U = make_user(Nick,CPid),
	NWorker = make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							[U|Worker#worker.users],
							Worker#worker.rooms),
	io:format("WORKER: register user ... ~p~n",[NWorker]),
	{noreply, NWorker};

handle_cast({add_user_to_room, RoomName, User}, Worker) ->
	%io:format("tu ~p~n", [self()]),
	Room = find_room(Worker#worker.rooms, RoomName),
	NewRoom = make_room(Room#room.name, self(), [User|Room#room.users], Room#room.msgs),
	io:format("User ~p entered ~p room~n", [User#user.nick, RoomName]),
	{noreply, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							Worker#worker.users,
							update_room(Worker#worker.rooms, NewRoom))};
							
handle_cast({room_msg,Nick,RName,Msg}, Worker) ->
	io:format("Room Msg incoming to ~p~n",[Worker]),
	Room = find_room(Worker#worker.rooms,RName),
	io:format("Room Msg incoming to ~p~n",[Room]),
	NRoom = add_room_msg(Room,make_msg(Nick,room,Msg)),
	notify_room(NRoom),
	{noreply, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							Worker#worker.users,
							update_room(Worker#worker.rooms, NRoom))};
							
handle_cast({message, ToNick, Msg}, Worker) ->
	ToPid = getPid(Worker#worker.users, ToNick),
	if ToPid =:= false ->
			%user je na inom workerovi
			ToWorker = gen_server:call(multichatapp, {wherenick, ToNick}),
			gen_server:cast(ToWorker, {message, ToNick, Msg});
		true ->
			io:format("WORKER: msg sent to recipient (~p)!~n", [ToPid]),
			gen_server:cast(ToPid, {message, Msg})
	end,
	{noreply, Worker};

handle_cast({notif_room,Nick,Room}, Worker) ->
	U = self_find_client(Worker#worker.users,Nick),
	io:format("WORKER: NewRoom FOR ~p~n",[U]),
	if U ->
		gen_server:cast(U#user.pid,{notif_room_update, Room})
	end,
	{noreply,Worker};

handle_cast(_, Worker) ->
	io:format("empty cast!~n"),
	{noreply, Worker}.
	
handle_info(Msg, Worker) ->
	io:format("WORKER: Unexpected message: ~p~n", [Msg]),
	{noreply, Worker}.
terminate(normal, _) ->
	io:format("Worker is shutting down...!~n"),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_worker(MyPid, ServerPid, Backup, Users, Rooms) ->
	#worker{mypid	= MyPid, 
			mainpid	= ServerPid,
			backup	= Backup, 
			users	= Users, 
			rooms	= Rooms}.
			
make_user(Nick, Pid) ->
	#user{nick=Nick, pid=Pid}.
	
make_room(Name, Pid, Users, Msgs) ->
	#room{name=Name, pid=Pid, users=Users, msgs=Msgs}.
	
make_msg(From,To,Text) ->
	#msg{ufrom=From, uto=To, text=Text}.

getPid([], _) -> false;
getPid([H|_], UserNick) when H#user.nick =:= UserNick ->
	H#user.pid;
getPid([_|T], UserNick) ->
	getPid(T, UserNick).


update_room(List, Room) ->
	update_room(List, [], Room).
	
update_room([], Acc, _) -> Acc;
update_room([H|T], Acc, Room) when H#room.name =:= Room#room.name ->
	[Room|Acc] ++ T;
update_room([H|T], Acc, Room) ->
	update_room(T, [H|Acc], Room).

add_msg_list([],M) ->
	[M];
add_msg_list(L,M) ->
	L ++ [M].

add_room_msg(R,Msg) ->
	#room{name=R#room.name, 
			pid=R#room.pid, 
			users=R#room.users,
			msgs = add_msg_list(R#room.msgs,Msg)}.

find_room([],_) ->
	false;
find_room([R|_], RoomName) when R#room.name =:= RoomName ->
	R;
find_room([_|T], RoomName) ->
	find_room(T, RoomName).
	
self_find_client([],Nick) ->
	false;
self_find_client([U|_],Nick) when U#user.nick =:= Nick ->
	U;
self_find_client([_|T],Nick) ->
	self_find_client(T,Nick).

notify_room_users([],Room) ->
	ok;
notify_room_users([U|T],Room) ->
	gen_server:cast(U#user.pid,{notif_room,U#user.nick,Room}),
	notify_room_users(T,Room).
	
notify_room(Room) ->
	notify_room_users(Room#room.users,Room).


