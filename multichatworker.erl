-module(multichatworker).
-behaviour(gen_server).
-export([create/1,
		sendtouser/3,
		die/1	
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(user, {nick, pid}).
-record(room, {name, pid}).
-record(worker, {mypid, spid, users}).

-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").

%%% Client API
create(ParentPid) -> 
	{ok, MyPid} = gen_server:start_link(?MODULE, ParentPid, []),
	MyPid.

sendtouser(MyPid, ToNick, Msg) ->
	% ToPid = getPid(ToNick), 
	gen_server:call(MyPid, {message, MyPid, ToNick, Msg}).

die(MyPid) ->
	gen_server:call(MyPid, terminate).

%%% Server functions
init(ParentPid) -> {ok, make_worker(0, ParentPid, [])}. %% no treatment of info here!
 
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
handle_call(terminate, _From, Worker) ->
	{stop, normal, ok, Worker}.	

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
make_worker(MyPid, ServerPid, Users) ->
	#worker{mypid=MyPid, spid=ServerPid, users=Users}.
