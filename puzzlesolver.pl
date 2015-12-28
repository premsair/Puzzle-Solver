
%%%%%%%%%Stack Implementation%%%%%%%%%%%%%
empty_stack([]).

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

member_stack(E, S) :- member(E, S).

stack(E, S, [E|S]).

reverse_print_stack(S) :-
	empty_stack(S).
reverse_print_stack(S) :-
	stack(E, Rest, S),
	reverse_print_stack(Rest),
	write(E), nl.


%%%%%%%%Queue Implementation%%%%%%%%%%%%%%%

empty_queue([]).

member_queue(E, S) :- member(E, S).

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :- 
    append(First, Second, Concatenation).


%%%%%%%Set Implementation%%%%%%%%%%%%%%%%%%

empty_set([]).

member_set(E, S) :- member(E, S).

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T) :- !.
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new), !.
    
union([], S, S).
union([H|T], S, S_new) :- 
    union(T, S, S2),
    add_to_set(H, S2, S_new).   
    
intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
    member_set(H, S),
    intersection(T, S, S_new),!.
intersection([_|T], S, S_new) :-
    intersection(T, S, S_new),!.
    
set_diff([], _, []).
set_diff([H|T], S, T_new) :- 
    member_set(H, S), 
    set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :- 
    set_diff(T, S, T_new), !.

subset([], _).
subset([H|T], S) :- 
    member_set(H, S), 
    subset(T, S).

equal_set(S1, S2) :- 
    subset(S1, S2), subset(S2, S1).

%%%%%%%%%%%Priority Queue Implementation%%%%%%%%

empty_sort_queue([]).

member_sort_queue(E, S) :- member(E, S).

insert_sort_queue(State, [], [State]).  
insert_sort_queue(State, [H | T], [State, H | T]) :- 
    precedes(State, H).
insert_sort_queue(State, [H|T], [H | T_new]) :- 
    insert_sort_queue(State, T, T_new). 
    
remove_sort_queue(First, [First|Rest], Rest).


%%%%%%%Depth First Search Implementation%%%%%%%%%%

go(Start,Goal,dfs,Puzzle) :-
	empty_stack(Empty_been_stack),
	stack(Start,Empty_been_stack,Been_stack),
	path(Start,Goal,Been_stack,dfs).

path(Goal,Goal,Been_stack,dfs) :-
	write('Solution Path Is:' ), nl,
	reverse_print_stack(Been_stack).

path(State,Goal,Been_stack,dfs) :-
	move(State,Next_state,Puzzle),
	not(member_stack(Next_state,Been_stack)),
	stack(Next_state,Been_stack,New_been_stack),
	path(Next_state,Goal,New_been_stack,dfs),!.


%%%%%%%%Breadth First Search Implementation%%%%%%%%%
state_record(State, Parent, [State, Parent]).

go(Start, Goal,bfs,Puzzle) :- 
    empty_queue(Empty_open),
    state_record(Start, nil, State),
    add_to_queue(State, Empty_open, Open),
    empty_set(Closed),
    path(Open, Closed, Goal,bfs,Puzzle).

path(Open,_,_,bfs,Puzzle) :- empty_queue(Open),
                  write('graph searched, no solution found').
    
path(Open, Closed, Goal,bfs,Puzzle) :- 
    remove_from_queue(Next_record, Open, _),
    state_record(State, _, Next_record),
    State = Goal,
    write('Solution path is: '), nl,
    printsolution(Next_record, Closed,bfs).
    
path(Open, Closed, Goal,bfs,Puzzle) :- 
    remove_from_queue(Next_record, Open, Rest_of_open),
    (bagof(Child, moves(Next_record, Open, Closed, Child,bfs,Puzzle), Children);Children = []),
    add_list_to_queue(Children, Rest_of_open, New_open), 
    add_to_set(Next_record, Closed, New_closed),
    path(New_open, New_closed, Goal,bfs,Puzzle),!.

moves(State_record, Open, Closed, Child_record,bfs,Puzzle) :-
    state_record(State, _, State_record),
    move(State, Next,Puzzle),
    state_record(Next, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record(Next, State, Child_record).

printsolution(State_record, _,bfs):- 
    state_record(State,nil, State_record),
    write(State), nl.
printsolution(State_record, Closed,bfs) :-
    state_record(State, Parent, State_record),
    state_record(Parent, _, Parent_record),
    member(Parent_record, Closed),
    printsolution(Parent_record, Closed,bfs),
    write(State), nl.
        
add_list_to_queue([], Queue, Queue).
add_list_to_queue([H|T], Queue, New_queue) :-
    add_to_queue(H, Queue, Temp_queue),
    add_list_to_queue(T, Temp_queue, New_queue).

%%%%%%%%Best First Search Implementation%%%%%%%%%%%%


state_record(State, Parent, G, H, F, [State, Parent, G, H, F]).
precedes([_,_,_,_,F1], [_,_,_,_,F2]) :- F1 =< F2.   
    
go(Start, Goal,hfs,Puzzle) :- 
    empty_set(Closed),
    empty_sort_queue(Empty_open),
    heuristic(Start, Goal, H,Puzzle),
    state_record(Start, nil, 0, H, H, First_record),
    insert_sort_queue(First_record, Empty_open, Open),
    path(Open,Closed, Goal,hfs,Puzzle).

path(Open,_,_,hfs,Puzzle) :- 
    empty_sort_queue(Open),
    write("graph searched, no solution found").

    
path(Open, Closed, Goal,hfs,Puzzle) :- 
    remove_sort_queue(First_record, Open, _),
    state_record(State, _, _, _, _, First_record),
    State = Goal,
    write('Solution path is: '), nl,
    printsolution(First_record, Closed,hfs).
    
path(Open, Closed, Goal,hfs,Puzzle) :- 
    remove_sort_queue(First_record, Open, Rest_of_open),
    (bagof(Child, moves(First_record, Open, Closed, Child, Goal,hfs,Puzzle), Children);Children = []),
    insert_list(Children, Rest_of_open, New_open),
    add_to_set(First_record, Closed, New_closed),
    path(New_open, New_closed, Goal,hfs,Puzzle),!.
    
moves(State_record, Open, Closed,Child, Goal,hfs,Puzzle) :-
    state_record(State, _, G, _,_, State_record),
    move(State, Next,Puzzle),
    state_record(Next, _, _, _, _, Test),
    not(member_sort_queue(Test, Open)),
    not(member_set(Test, Closed)),
    cost(State,Next,G,G_new,Puzzle),
    heuristic(Next, Goal, H,Puzzle),
    F is G_new + H,
    state_record(Next, State, G_new, H, F, Child).
    
insert_list([], L, L).
insert_list([State | Tail], L, New_L) :-
    insert_sort_queue(State, L, L2),
    insert_list(Tail, L2, New_L).

printsolution(Next_record, _,hfs):-  
    state_record(State, nil, _, _,_, Next_record),
    write(State), nl.

printsolution(Next_record, Closed,hfs) :-
    state_record(State, Parent, _, _,_, Next_record),
    state_record(Parent, _, _, _, _, Parent_record),
    member_set(Parent_record, Closed),
    printsolution(Parent_record, Closed,hfs),
    write(State), nl.

%%%%%%%%%%%%%%%%Vampires,Werewolves problem%%%%%%%%%%%

legal(WE,VE,WW,VW) :-
	VE>=0, WE>=0, VW>=0, WW>=0,
	(VE>=WE ; VE=0),
	(VW>=WW ; VW=0).

move([WE,VE,east,WW,VW],[WE,VE2,west,WW,VW2],vamp-wolf):-
	% Two vampires cross east to west.
	VW2 is VW+2,
	VE2 is VE-2,
	legal(WE,VE2,WW,VW2).

move([WE,VE,east,WW,VW],[WE2,VE,west,WW2,VW],vamp-wolf):-
	% Two werewolves cross east to west.
	WW2 is WW+2,
	WE2 is WE-2,
	legal(WE2,VE,WW2,VW).

move([WE,VE,east,WW,VW],[WE2,VE2,west,WW2,VW2],vamp-wolf):-
	%  One vampire and one werewolf cross east to west.
	WW2 is WW+1,
	WE2 is WE-1,
	VW2 is VW+1,
	VE2 is VE-1,
	legal(WE2,VE2,WW2,VW2).

move([WE,VE,east,WW,VW],[WE,VE2,west,WW,VW2],vamp-wolf):-
	% One vampire crosses east to west.
	VW2 is VW+1,
	VE2 is VE-1,
	legal(WE,VE2,WW,VW2).

move([WE,VE,east,WW,VW],[WE2,VE,west,WW2,VW],vamp-wolf):-
	% One werewolf crosses east to west.
	WW2 is WW+1,
	WE2 is WE-1,
	legal(WE2,VE,WW2,VW).

move([WE,VE,west,WW,VW],[WE,VE2,east,WW,VW2],vamp-wolf):-
	% Two vampires cross west to east.
	VW2 is VW-2,
	VE2 is VE+2,
	legal(WE,VE2,WW,VW2).

move([WE,VE,west,WW,VW],[WE2,VE,east,WW2,VW],vamp-wolf):-
	% Two werewolves cross west to east.
	WW2 is WW-2,
	WE2 is WE+2,
	legal(WE2,VE,WW2,VW).

move([WE,VE,west,WW,VW],[WE2,VE2,east,WW2,VW2],vamp-wolf):-
	%  One vampire and one werewolf cross west to east.
	WW2 is WW-1,
	WE2 is WE+1,
	VW2 is VW-1,
	VE2 is VE+1,
	legal(WE2,VE2,WW2,VW2).

move([WE,VE,west,WW,VW],[WE,VE2,east,WW,VW2],vamp-wolf):-
	% One vampire crosses west to east.
	VW2 is VW-1,
	VE2 is VE+1,
	legal(WE,VE2,WW,VW2).

move([WE,VE,west,WW,VW],[WE2,VE,east,WW2,VW],vamp-wolf):-
	% One werewolf crosses west to east.
	WW2 is WW-1,
	WE2 is WE+1,
	legal(WE2,VE,WW2,VW).


%%%%%%%%%BlackWhite Tile Problem%%%%%%%%%%%%%%%%%%%%

move([x,W1,W2,W3,B1,B2,B3],[W1,x,W2,W3,B1,B2,B3],black-white).
move([x,W1,W2,W3,B1,B2,B3],[W2,W1,x,W3,B1,B2,B3],black-white).
move([x,W1,W2,W3,B1,B2,B3],[W3,W1,W2,x,B1,B2,B3],black-white).

move([W1,x,W2,W3,B1,B2,B3],[x,W1,W2,W3,B1,B2,B3],black-white).
move([W1,x,W2,W3,B1,B2,B3],[W1,W2,x,W3,B1,B2,B3],black-white).
move([W1,x,W2,W3,B1,B2,B3],[W1,W3,W2,x,B1,B2,B3],black-white).
move([W1,x,W2,W3,B1,B2,B3],[W1,B1,W2,W3,x,B2,B3],black-white).

move([W1,W2,x,W3,B1,B2,B3],[x,W2,W1,W3,B1,B2,B3],black-white).
move([W1,W2,x,W3,B1,B2,B3],[W1,x,W2,W3,B1,B2,B3],black-white).
move([W1,W2,x,W3,B1,B2,B3],[W1,W2,W3,x,B1,B2,B3],black-white).
move([W1,W2,x,W3,B1,B2,B3],[W1,W2,B1,W3,x,B2,B3],black-white).
move([W1,W2,x,W3,B1,B2,B3],[W1,W2,B2,W3,B1,x,B3],black-white).

move([W1,W2,W3,x,B1,B2,B3],[x,W2,W3,W1,B1,B2,B3],black-white).
move([W1,W2,W3,x,B1,B2,B3],[W1,x,W3,W2,B1,B2,B3],black-white).
move([W1,W2,W3,x,B1,B2,B3],[W1,W2,x,W3,B1,B2,B3],black-white).
move([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B1,x,B2,B3],black-white).
move([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B2,B1,x,B3],black-white).
move([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B3,B1,B2,x],black-white).

move([W1,W2,W3,B1,x,B2,B3],[W1,x,W3,B1,W2,B2,B3],black-white).
move([W1,W2,W3,B1,x,B2,B3],[W1,W2,x,B1,W3,B2,B3],black-white).
move([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,x,B1,B2,B3],black-white).
move([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,B1,B2,x,B3],black-white).
move([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,B1,B3,B2,x],black-white).


move([W1,W2,W3,B1,B2,x,B3],[W1,W2,x,B1,B2,W3,B3],black-white).
move([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,x,B2,B1,B3],black-white).
move([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,B1,x,B2,B3],black-white).
move([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,B1,B2,B3,x],black-white).

move([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,x,B2,B3,B1],black-white).
move([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,B1,x,B3,B2],black-white).
move([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,B1,B2,x,B3],black-white).


%%%%%%%%%%Heuristics for vamp-wolf puzzle%%%%%%%%%%%%%%%

heuristic([WE,VE,LR,WW,VW],Goal,H,vamp-wolf):- H is (6-WW-VW).
cost(X,Y,G,G_new,vamp-wolf):- G_new is G+1.

%%%%%%%%%%Heuristics for black-white puzzle%%%%%%%%%%%%%%%

heuristic([X1,X2,X3,X4,X5,X6,X7],Goal,H,black-white):- H is 0,value([X1,X2],H),value([X1,X3],H),value([X2,X3],H),value([X1,X4],H),value([X2,X4],H),value([X3,X4],H),value([X1,X5],H),
						       value([X2,X5],H),value([X3,X5],H),value([X4,X5],H),value([X1,X6],H),value([X2,X6],H),value([X3,X6],H),value([X4,X6],H),
						       value([X5,X6],H),value([X1,X7],H),value([X2,X7],H),value([X3,X7],H),value([X4,X7],H),value([X5,X7],H),value([X6,X7],H).


value([b,w],H):- H is H+1.
value([_,_],H):- H is H+0.
	
cost([x,W1,W2,W3,B1,B2,B3],[W1,x,W2,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([x,W1,W2,W3,B1,B2,B3],[W2,W1,x,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([x,W1,W2,W3,B1,B2,B3],[W3,W1,W2,x,B1,B2,B3], G, G_new,black-white):- G_new is G+2.

cost([W1,x,W2,W3,B1,B2,B3],[x,W1,W2,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,x,W2,W3,B1,B2,B3],[W1,W2,x,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,x,W2,W3,B1,B2,B3],[W1,W3,W2,x,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,x,W2,W3,B1,B2,B3],[W1,B1,W2,W3,x,B2,B3], G, G_new,black-white):- G_new is G+2.

cost([W1,W2,x,W3,B1,B2,B3],[x,W2,W1,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,x,W3,B1,B2,B3],[W1,x,W2,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,x,W3,B1,B2,B3],[W1,W2,W3,x,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,x,W3,B1,B2,B3],[W1,W2,B1,W3,x,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,x,W3,B1,B2,B3],[W1,W2,B2,W3,B1,x,B3], G, G_new,black-white):- G_new is G+2.

cost([W1,W2,W3,x,B1,B2,B3],[x,W2,W3,W1,B1,B2,B3], G, G_new,black-white):- G_new is G+2.
cost([W1,W2,W3,x,B1,B2,B3],[W1,x,W3,W2,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,x,B1,B2,B3],[W1,W2,x,W3,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B1,x,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B2,B1,x,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,x,B1,B2,B3],[W1,W2,W3,B3,B1,B2,x], G, G_new,black-white):- G_new is G+2.

cost([W1,W2,W3,B1,x,B2,B3],[W1,x,W3,B1,W2,B2,B3], G, G_new,black-white):- G_new is G+2.
cost([W1,W2,W3,B1,x,B2,B3],[W1,W2,x,B1,W3,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,x,B1,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,B1,B2,x,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,x,B2,B3],[W1,W2,W3,B1,B3,B2,x], G, G_new,black-white):- G_new is G+1.


cost([W1,W2,W3,B1,B2,x,B3],[W1,W2,x,B1,B2,W3,B3], G, G_new,black-white):- G_new is G+2.
cost([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,x,B2,B1,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,B1,x,B2,B3], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,B2,x,B3],[W1,W2,W3,B1,B2,B3,x], G, G_new,black-white):- G_new is G+1.

cost([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,x,B2,B3,B1], G, G_new,black-white):- G_new is G+2.
cost([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,B1,x,B3,B2], G, G_new,black-white):- G_new is G+1.
cost([W1,W2,W3,B1,B2,B3,x],[W1,W2,W3,B1,B2,x,B3], G, G_new,black-white):- G_new is G+1.

