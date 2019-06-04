% Team Members: Michael Zavalza, Kennedy Vandel

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

% Exercise_1

schedule(W,P,T) :- enroll(W,C) , when(C,T), where(C,P).

usage(P,T) :- where(C,P), when(C,T).

conflict(X,Y) :- where(X,L), when(X,T), where(Y,L), when(Y,T), X \= Y.

meet(X,Y) :- enroll(X,C), enroll(Y,C), X \= Y.
meet(X,Y) :- enroll(X,C), where(C,P), when(C,T), enroll(Y,D), where(D,P), when(D,S), S =:= T+1, X \= Y.

% Exercise_2

rdup([],[]).
rdup([X],[X]).
rdup([X,X|L],M) :- rdup([X|L],M).
rdup([X,Y|L],[X|M]) :- rdup([Y|L],M), X \= Y.

flat([],[]).
flat([X|M],L) :- append(X,M,Y), flat(Y,L).
flat([X|M],[X|L]) :- \+ is_list(X), flat(M,L).

%project(X,Y,Z) :-
