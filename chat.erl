-module(chat).
-behaviour(gen_server).
-export([login/1,
		logout/1,
		roomlist/0,
		newroom/1,	
		enter/1,
		exit/0,	
		msg/2,
		send/1
		]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(person, {name=rado, weight=80}).
-author("Bc. Radoslav Zachar").
-author("Bc. Jakub Calik").
-date("1.4.2013").

%%% Client API
create() -> 
	gen_server:start_link(?MODULE, [], []).

person(Name, Weight) ->
	make_person(Name, Weight).

enter(Pid, Name, Weight) ->
	enter(Pid, make_person(Name, Weight)).
	
enter(Pid, NewPerson) ->
	gen_server:call(Pid, {enter, NewPerson}).
	
leave(Pid, Name, Weight) ->
	leave(Pid, make_person(Name, Weight)).

leave(Pid, Person) ->
	gen_server:call(Pid, {leave, Person}).
	
take(Pid) ->
	gen_server:cast(Pid, {take}).
 
destroy(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}. %% no treatment of info here!
 
handle_call({enter, NewPerson}, _From, Elevator) ->
	TotalWeight = weight_in_elevator(Elevator) + NewPerson#person.weight, 
	if TotalWeight > 300 ->
			% person cant enter an elevator due to weight limit
			io:format("Person cant fit to this elevator. Please take all persons first or create another elevator!~n",[]),
			NewElevator = Elevator;
		true ->
			NewElevator = insert(Elevator, NewPerson)		
	end,
	io:format("Actual total weight in elevator: ~p~n",[weight_in_elevator(NewElevator)]),
	{reply, NewElevator, NewElevator};
handle_call({leave, Person}, _From, Elevator) ->
	IsInElevator = isin(Elevator,Person),
	if IsInElevator == true ->
			io:format("Person ~p left this elevator.~n",[Person#person.name]),
			NewElevator = delete(Elevator, Person);
		true ->
			io:format("Person ~p is not is elevator!~n",[Person#person.name]),
			NewElevator = Elevator
	end,
	io:format("Actual total weight in elevator: ~p~n",[weight_in_elevator(NewElevator)]),
	{reply, NewElevator, NewElevator};
handle_call(terminate, _From, Elevator) ->
	{stop, normal, ok, Elevator}.	

handle_cast({take}, Elevator) ->
	[io:format("Person ~p was taken.~n",[P#person.name]) || P <- Elevator],
	io:format("This elevator is empty now!~n",[]),
	{noreply, []}.

handle_info(Msg, Elevator) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Elevator}.

terminate(normal, Elevator) ->
	[io:format("Person ~p was taken.~n",[P#person.name]) || P <- Elevator],
	io:format("This elevator cant take people anymore!~n",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
make_person(Name, Weight) ->
	#person{name=Name, weight=Weight}.

% zistime momentalnu zataz vytahu
weight_in_elevator(Elevator) ->
	count_weight_in_elevator(Elevator, 0).
	
count_weight_in_elevator([], Total) ->
	Total;
count_weight_in_elevator([H|T], Total) ->
	count_weight_in_elevator(T, Total + H#person.weight).

% zistime, ci osoba je vo vytahu
isin([],_) ->
	false;
isin([P|_], Person) when P#person.weight > Person#person.weight ->
	false;
isin([P|_], Person) when P =:= Person ->
	true;
isin([_|T], Person) ->
	isin(T, Person).
  
% pridame osobu do vytahu (zoradujeme ich podla vahy:))
insert(Elevator, Person) ->
	insert(Elevator, Person , []).
  
insert([P|T], Person, Acc) when P#person.weight < Person#person.weight ->
	insert(T, Person, Acc ++ [P]);
insert(Elevator, Person, Acc) ->
	Acc ++ [Person] ++ Elevator.

% odobereme osobu z vytahu
delete(Elevator, Ele) ->
  delete(Elevator, Ele, []).
  
delete([], _, Acc) ->
  Acc;
delete([P|T], Person, Acc) when P =:= Person ->
  Acc ++ T;
delete([P|T], Person, Acc) ->
  delete(T, Person, Acc ++ [P]).
    