-module(multichatworker).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-record(user, {nick, pid}).
-record(room, {name, pid, users}).
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
% send message
handle_call({message, FromPid, ToPid, Msg}, _From, Worker) when FromPid =:= Worker#worker.mypid ->
	% todo = vytiahnut pid z mojej databazy
	% ToPid = getPid(ToPid),
	ToPid = 5,
	%gen_server:call(ToPid, {message, MyPid, ToPid, Msg}),
	io:format("Msg sent (to ~p)! ~p~n", [ToPid, Msg]),
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
	Room = make_room(RoomName, self(), []),
	gen_server:call(multichatapp, {newroomonpid, RoomName, Worker#worker.mypid}),
	{reply, ok, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							Worker#worker.users,
							[Room|Worker#worker.rooms])};

handle_call(terminate, _From, Worker) ->
	{stop, normal, ok, Worker}.	

handle_cast({add_user_to_room, RoomName, User}, Worker) ->
	%io:format("tu ~p~n", [self()]),
	Room = find_room(Worker#worker.rooms, RoomName),
	NewRoom = make_room(Room#room.name, self(), [Room#room.users|User]),
	io:format("User ~p entered ~p room~n", [User#user.nick, RoomName]),
	{noreply, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.backup,
							Worker#worker.users,
							update_room(Worker#worker.rooms, NewRoom))};

handle_cast(_, Worker) ->
	io:format("empty cast!~n", []),
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
	
make_room(Name, Pid, Users) ->
	#room{name=Name, pid=Pid, users=Users}.

update_room(List, Room) ->
	update_room(List, [], Room).
	
update_room([], Acc, _) -> Acc;
update_room([H|T], Acc, Room) when H#room.name =:= Room#room.name ->
	[[Acc|Room]|T];
update_room([H|T], Acc, Room) ->
	update_room(T, [Acc|H], Room).

find_room([],_) ->
	false;
find_room([R|_], RoomName) when R#room.name =:= RoomName ->
	R;
find_room([_|T], RoomName) ->
	find_room(T, RoomName).

	



