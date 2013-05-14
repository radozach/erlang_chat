-module(multichatworker).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-record(user, {nick, pid}).
-record(room, {name, pid, users}).
-record(worker, {mypid, mainpid, users, rooms}).
-export([
%		create/1,
		sendtouser/3,
		die/1	
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Client API
%create(ParentPid) -> 
%	gen_server:start_link(?MODULE, ParentPid, []).
	
sendtouser(MyPid, ToNick, Msg) ->
	% ToPid = getPid(ToNick), 
	gen_server:call(MyPid, {message, MyPid, ToNick, Msg}).

die(MyPid) ->
	gen_server:call(MyPid, terminate).

%%% Server functions
init(ParentPid) ->
	MyPid = self(), 
	{ok, make_worker(MyPid, ParentPid, [], [])}. 
 
% receive message
handle_call({message, FromPid, ToPid, Msg}, _From, Worker) when ToPid =:= Worker#worker.mypid ->
	% todo = zistit nick pidu odkial prisla sprava (opytat sa servera ak nemam ulozeneho)
	% FromNick = getNick(FromPid),
	FromNick = noname,
	io:format("Received msg (from ~p): ~p~n",[FromNick,Msg]),
	{reply, Worker, Worker};
% send message
handle_call({message, FromPid, ToPid, Msg}, _From, Worker) when FromPid =:= Worker#worker.mypid ->
	% todo = vytiahnut pid z mojej databazy
	% ToPid = getPid(ToPid),
	ToPid = 5,
	%gen_server:call(ToPid, {message, MyPid, ToPid, Msg}),
	io:format("Msg sent (to ~p)! ~p~n",[ToPid, Msg]),
	{reply, Worker, Worker};
	
handle_call({enter_room,RoomName,UserNick}, _From, Worker) ->
	R = get_room(Worker,RoomName),
	if R =:= false ->
			
			RoomPid = gen_server:call(multichatapp,{whereroom,RoomName}),
			NewWorker = Worker;
		true ->
			RoomPid = R#room.pid,
			NewWorker = Worker
	end,
	io:format("Room at ~p~n",[RoomPid]),
	gen_server:cast(RoomPid,{user_entered,RoomName,UserNick}),
	{reply, RoomPid, NewWorker};
	

							
handle_call({make_room,RoomName}, _From, Worker) ->
	Room = make_room(RoomName,[]),
	io:format("Room createeeed"),
	gen_server:call(multichatapp,{newroomonpid, RoomName, Worker#worker.mypid}),
	{reply, ok, make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.users,
							[Room|Worker#worker.rooms])};


handle_call(terminate, _From, Worker) ->
	{stop, normal, ok, Worker}.	

handle_cast({user_entered,RoomName,UserNick},Worker) ->
	io:format("tu ~p~n",[self()]),
	Room = find_room(Worker#worker.rooms,RoomName),
	io:format("Room ~p~n",[Room]),
	NewRoom = make_room(Room#room.name,[UserNick|Room#room.users]),
	NewRooms = update_room(Worker#worker.rooms,[],NewRoom),
	io:format("Worker register user in room~n"),
	{noreply,make_worker(Worker#worker.mypid,
							Worker#worker.mainpid,
							Worker#worker.users,
							NewRooms)};

handle_cast(_, Worker) ->
	io:format("empty cast!~n",[]),
	{noreply, Worker}.
	
handle_info(Msg, Worker) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Worker}.

terminate(normal, Worker) ->
	io:format("Worker is shutting down...!~n",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_room(Name,Users) ->
	io:format("NEW room ~n",[]),
	#room{name=Name, pid=self(), users=Users}.

update_room([],A,_) ->
	A;
update_room([H|T],A,R) when H#room.name =:= R#room.name ->
	update_room(T,[R|A],R);
update_room([H|T],A,R) ->
	update_room(T,[H|A],R).

make_worker(MyPid, ServerPid, Users, Rooms) ->
	#worker{mypid=MyPid, mainpid=ServerPid, users=Users, rooms=Rooms}.
	

get_room(Worker,RM) ->
	find_room(Worker#worker.rooms,RM).
	

find_room([],_) ->
	false;
find_room([R|_],RM) when R#room.name =:= RM ->
	R;
find_room([_|T],RM) ->
	find_room(T,RM).

	



