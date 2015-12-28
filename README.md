# Puzzle-Solver
Black White and Vampires Werewolves Problem Solver

README

Prolog file that has all the code integrated into one is puzzle_solver.pl
Prolog code is developed in SWI-Prolog.

BFS,DFS,HFS snippets are taken from the luger's textbook and modified to our purpose.

To execute the prolog file:

swipl

% Command to compile the prolog file
?-[puzzle_solver]. or ?-consult(puzzle_solver).

% General format to submit the puzzle
% ?-go(State,Goal/Otherstate,Search_strategy,Puzzle).

% To run Vamp-Wolf puzzle using DFS approach
?- go([3,3,east,0,0],[0,0,west,3,3],dfs,vamp-wolf).

% To run Vamp-Wolf puzzle using BFS approach
?- go([3,3,east,0,0],[0,0,west,3,3],bfs,vamp-wolf).

% To run Vamp-Wolf puzzle using Heuristic approach
?- go([3,3,east,0,0],[0,0,west,3,3],hfs,vamp-wolf).

% To run Black-White puzzle using BFS approach
?-go([b,b,b,x,w,w,w],[w,w,w,x,b,b,b],bfs,black-white).

% To run Black-White puzzle using DFS approach
?-go([b,b,b,x,w,w,w],[w,w,w,x,b,b,b],dfs,black-white).

% To run Black-White puzzle using Heuristic approach
?-go([b,b,b,x,w,w,w],[w,w,w,x,b,b,b],hfs,black-white).
