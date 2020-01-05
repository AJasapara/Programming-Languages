unique_list(List, N) :-
	length(List, N),
    fd_domain(List, 1, N),
	fd_all_different(List).

transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).

transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix), transpose(RestMatrix, Rows).

check_count([], _, 0).
check_count([FirstTower|RestTowers], High, Count) :-
	High #=< FirstTower,
	Count #> 0,
	CountLessOne #= Count - 1,
	check_count(RestTowers, FirstTower, CountLessOne).
check_count([FirstTower|RestTowers], High, Count) :-
	High #> FirstTower,
	check_count(RestTowers, High, Count).

check_direction([], _, _).
check_direction([FirstRow|RestRows], [FirstCountOne|RestCountOnes], [FirstCountTwo|RestCountTwos]) :-
    check_count(FirstRow, 1, FirstCountOne),
    reverse(FirstRow, ReverseFirstRow),
    check_count(ReverseFirstRow, 1, FirstCountTwo),
    check_direction(RestRows, RestCountOnes, RestCountTwos).

check_tower(N, T, counts(Top, Bottom, Left, Right)) :-
    length(Top, N), length(Bottom, N), length(Left, N), length(Right, N),
    check_direction(T, Left, Right),
    transpose(T, Transpose),
    check_direction(Transpose, Top, Bottom).

tower(N, T, C) :-
	length(T, N),
	length(Ns, N),
	fd_domain(Ns, N, N),
	maplist(unique_list, T, Ns),
	transpose(T,Transpose),
    maplist(unique_list, Transpose, Ns),
    maplist(fd_labeling, T),
	check_tower(N, T, C).

values_list([], 0).
values_list(List, N) :-
    append([N], RestList, List),
    N > 0,
    NLessOne is N - 1,
    values_list(RestList, NLessOne).

unique_list2([], _).
unique_list2([FirstList|RestList], List) :-
    maplist(\==, FirstList, List),
    unique_list2(RestList, List).

check_uniqueness([], _, _, _).
check_uniqueness([FirstList|RestList], N, Values, SeenValues) :-
	length(FirstList, N),
	permutation(FirstList, Values),
	unique_list2(SeenValues, FirstList),
	append(SeenValues, [FirstList], NextSeenValues),
	check_uniqueness(RestList, N, Values, NextSeenValues).

plain_tower(N, T, C) :-
    values_list(Values, N), !,
    length(T, N),
	check_uniqueness(T, N, Values, []),
	transpose(T, Transpose),
	check_uniqueness(Transpose, N, Values, []),
	check_tower(N, T, C).

speedup(R) :-
	statistics(cpu_time,[Start|_]),
	tower(5, _, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
	statistics(cpu_time,[Stop|_]),
	Time is Stop - Start,
	statistics(cpu_time,[Start_Plain|_]),
	plain_tower(5, _, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
	statistics(cpu_time,[Stop_Plain|_]),
	Time_Plain is Stop_Plain - Start_Plain,
	R is Time_Plain / Time.

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \== T2.
