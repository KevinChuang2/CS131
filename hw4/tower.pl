tower(N,T,C):-
    length(T,N),
    list_lengths(T,N),
    list_within_bound(T,N),
    % check every number in row to make sure different
    maplist(fd_all_different, T),
    transpose(T, X),
    maplist(fd_all_different,X),
    %assign value to each variable in T, necessary or else type error.
    maplist(fd_labeling, T),
    reverse_all(T, Trev),
    reverse_all(X, Xrev),
    counts(U,D,L,R) = C,
    length(U,N),
    length(D,N),
    length(L,N),
    length(R,N),
    %check each count and make sure matches up to what towers you can see
    verify_row(T,L),
    verify_row(X,U),
    verify_row(Trev, R),
    verify_row(Xrev, D).

%check each list and make sure it is of length N
list_lengths([], _).
list_lengths([H|T], N) :-
        length(H, N),
        list_lengths(T, N).

%use fd to make sure that each variable within the list is within the our bound, N
list_within_bound([],_).
list_within_bound([H|T], U):-
    fd_domain(H, 1, U),
    list_within_bound(T,U).

%tranpose/2 takes two lists and uses the arity three version
%https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%takes a list and verifies a count
%pass in our first tower we see as our max
verify([H|T],C) :-
    verify(T,C,1, H).
%finished when the towers we can see match the count variable
verify([],N,N, _).
%case where the next tower is more than our max
%add to our count and keep checking the rest
verify([H|T], N, Count, CurrentMax):-
    H>CurrentMax,
    Count1 is Count+1,
    verify(T, N, Count1, H).
%case where our next tower is less, we can't see the tower,
%don't add to our count and keep checking the rest
verify([H|T], N, Count, CurrentMax):-
    H<CurrentMax,
    verify(T, N, Count, CurrentMax).

%check every row to a count array
verify_row([],[]).
verify_row([Hs|Ts],[C|Ct]):-
    verify(Hs,C),
    verify_row(Ts,Ct).

reverse_matrix([],[]).
reverse_matrix([H|T],[H2|T2]):-
    reverse(H,H2),
    reverse_matrix(T,T2).
    
% Reverse all rows
reverse_all([], []).
reverse_all([T | Ts], [Tr | Trs]) :-
	reverse(T, Tr),
	reverse_all(Ts, Trs).

    
%plain tower implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

different([], _).

%make sure H isn't in P, our array of stuff we've seen
%add H to our array of stuff we've seen, call it on rest of list
different([H | T], P) :- 
	\+ member(H, P),
	different(T, [H | P]).

%replaces fd_all_different
different(A) :- different(A, []).

%makes a list with all numbers between 1 and Max
make_list(Max, L):-
    findall(N, between(1,Max,N), L).

%generates 2d list
make_2d_list([],_).
make_2d_list([H|T], N):-
    make_list(N,X),
    permutation(X, H),
    make_2d_list(T,N).

%same thing as plain_tower, just replaced fd_all_different with different and generated the 2d list instead of fd_labeling
plain_tower(N,T,C):-
    length(T,N),
    list_lengths(T,N),
    make_2d_list(T,N),
    transpose(T, X),
    maplist(different,X),
    reverse_all(T, Trev),
    reverse_all(X, Xrev),
    counts(U,D,L,R) = C,
    length(U,N),
    length(D,N),
    length(L,N),
    length(R,N),
    verify_row(T,L),
    verify_row(X,U),
    verify_row(Trev, R),
    verify_row(Xrev, D).
    
%plain tower too slow for N=5 arrays
%randomly generated from towers puzzle website
get_plain_time(PlainTime) :-
	statistics(cpu_time, [_,_]),
	plain_tower(4,T,counts([2,2,1,2],[2,1,4,3],[3,4,1,2],[2,1,2,2])),
	statistics(cpu_time, [PlainEnd,_]),
	PlainTime is PlainEnd.


get_tower_time(TowerTime) :-
	statistics(cpu_time, [TowerStart,_]),
	tower(4,T,counts([2,2,1,2],[2,1,4,3],[3,4,1,2],[2,1,2,2])),
	statistics(cpu_time, [TowerEnd,_]),
	TowerTime is TowerEnd.


speedup(Ratio) :-
	get_tower_time(TowerTime),
	get_plain_time(PlainTime),
	Ratio is PlainTime/TowerTime.

ambiguous(N,C,T1,T2):-
    tower(N,T1,C),
    tower(N,T2,C),
    T1 \=T2.
    