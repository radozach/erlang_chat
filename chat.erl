-module(chat).
-behaviour(gen_server).
-author("Bc. Radoslav Zachar, Bc. Jakub Calik").
-date("1.4.2013").
-export([create/0, die/1,
		login/1,
		enter/2, exit/0,	
		msg/2
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(client, {nick, worker}).


%%% Client API
create() -> 
	gen_server:start_link(?MODULE, [], []).

login(Nick) -> 
	Resp = gen_server:call(multichatapp, {login, Nick}),
	 Logged = check_login(Resp),
	if Logged =:= true ->
		   %save worker pid
			gen_server:cast(self(), {save_worker, Nick, Resp}),
			io:format("Client logged.~n",[]);
	   true ->
		   io:format("Loging failed.")
	end.

enter(Pid, RoomName) ->
	gen_server:call(Pid, {enter, RoomName}).
	
exit() ->
	todo.

msg(To, Msg) ->
	todo.
 
die(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> 
	{ok, make_client()}. 
 
handle_call({enter, RoomName}, _From, Client) ->
	io:format("you are now in '~p' room!~n",[RoomName]),
	{reply, Client, Client};
handle_call(terminate, _From, Client) ->
	{stop, normal, ok, Client}.	

handle_cast({save_worker, Nick, WorkerPid}, _Client) ->
	NewClient = make_client(Nick,WorkerPid),
	io:format("This elevator is empty now!~n",[]),
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
	#client{nick=empty, worker=empty}.
make_client(Nick, WorkerPid) ->
	#client{nick=Nick, worker=WorkerPid}.

check_login(false) ->
	false;
check_login(_) ->
	true.



    