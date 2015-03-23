
%% based off 'An Iterated Subdivision Algorithm For Procedural Road Plan Generation' by Rudzicz and Verbrugge
:- module(citygen,
	[
		generate_roads/5
	]).
:- use_module(utils).

merge([], X, X).

merge(X, Y, [X|Y]) :-
	ground(X).

merge(X, Y, Z) :-
	append(X, Y, Z).

get_allotted([], [], []).
get_allotted([], Y, [Y]).
get_allotted(X, [], [X]).
get_allotted(X, Y, [X, Y]).

empty([]).

generate_roads(CitySize, MinArea, MaxArea, Allotted, Roads) :-
	MaxArea < 4,
	write('Max area must be greater than 3 (else 2x2 squares cannot be further subdivided).'), nl, !, fail
	;
	generate_road_network(MinArea, MaxArea, [CitySize], AllottedUnfiltered, Roads),
	exclude(empty, AllottedUnfiltered, Allotted),
	length(Allotted, LA),
	length(Roads, LR),
	format('Generated ~w areas and ~w roads', [LA, LR]), !.

generate_road_network(_, _, [], [], []) :-
	write('Done!').

generate_road_network(MinArea, MaxArea, [[]|Os], As, Roads) :-
	generate_road_network(MinArea, MaxArea, Os, As, Roads).

generate_road_network(MinArea, MaxArea, [O|Os], [A1, A2|As], [LBisection|Roads]) :-
	get_bisector(O, LBisection),
	bisect(O, LBisection, MinArea, P1, P2), !,
	format('~w bisected into ~w and ~w', [O, P1, P2]), nl,
	classify_bisection(P1, MaxArea, Oversized1, A1),
	classify_bisection(P2, MaxArea, Oversized2, A2), !,
	generate_road_network(MinArea, MaxArea, [Oversized1, Oversized2|Os], As, Roads).

classify_bisection(Area, Max, [], Area) :-
	area_size(Area, Size),
	Size =< Max.

classify_bisection(Area, Max, Area, []) :-
	area_size(Area, Size),
	Size > Max.

area_size(area(X1, X2, Y1, Y2), Size) :-
	area_size(X1, X2, Y1, Y2, Size).

area_size(X1, X2, Y1, Y2, Size) :-
	XSize is X2-X1,
	YSize is Y2-Y1,
	Size is XSize * YSize.

area_ratio(X1, X2, Y1, Y2, Ratio) :-
	XSize is X2-X1,
	YSize is Y2-Y1,
	Ratio is XSize / YSize.

bisect(area(X1, X2, Y1, Y2), area(_, _, BY1, BY2), Min, area(X1, X2, Y1, BY1), area(X1, X2, BY2, Y2)) :-
	BY2 is BY1 + 1,
	area_size(X1, X2, Y1, BY1, Size1),
	area_size(X1, X2, BY2, Y2, Size2),
	area_ratio(X1, X2, Y1, BY1, Ratio1),
	area_ratio(X1, X2, BY2, Y2, Ratio2),
	Ratio1 < 15,
	Ratio2 < 15,
	Size1 >= Min,
	Size2 >= Min.

bisect(area(X1, X2, Y1, Y2), area(BX1, BX2, _, _), Min, area(X1, BX1, Y1, Y2), area(BX2, X2, Y1, Y2)) :-
	BX2 is BX1 + 1,
	area_size(X1, BX1, Y1, Y2, Size1),
	area_size(BX2, X2, Y1, Y2, Size2),
	area_ratio(X1, BX1, Y1, Y2, Ratio1),
	area_ratio(BX2, X2, Y1, Y2, Ratio2),
	Ratio1 > 0.13,
	Ratio2 > 0.13,
	Size1 >= Min,
	Size2 >= Min.

get_bisector(area(X1, X2, Y1, Y2), area(X1, X2, BY1, BY2)) :-
	H is Y2-Y1,
	H >= 3,
	maybe, %% bisect it horizontally
	InnerY2 is Y2-2,
	InnerY1 is Y1+1,
	InnerY1 =< InnerY2,
	random_between(InnerY1,InnerY2, BY1),
	BY2 is BY1 + 1.

get_bisector(area(X1, X2, Y1, Y2), area(BX1, BX2, Y1, Y2)) :-	
	InnerX2 is X2-2,
	InnerX1 is X1+1,
	InnerX1 =< InnerX2,
	random_between(InnerX1, InnerX2, BX1),
	BX2 is BX1 + 1.

get_bisector(Area, Bisector):-
	get_bisector(Area, Bisector).