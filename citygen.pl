
%% based off 'An Iterated Subdivision Algorithm For Procedural Road Plan Generation' by Rudzicz and Verbrugge

:- use_module(utils).

merge([], X, X).

merge(X, Y, [X|Y]) :-
	ground(X).

merge(X, Y, Z) :-
	append(X, Y, Z).

get_allotted([], [], []).
get_allotted([], Y, Y).
get_allotted(X, [], X).
get_allotted(X, Y, [X, Y]).



generate_roads(CitySize, MinArea, MaxArea, Allotted, Roads) :-
	generate_road_network(MinArea, MaxArea, [CitySize], Allotted, Roads), 
	length(Allotted, LA),
	length(Allotted, LR),
	format('Generated ~w areas and ~w roads', [LA, LR]),
	!.

generate_road_network(_, _, [], [], []) :-
	write('Done!').

generate_road_network(MinArea, MaxArea, [O|Os], Allotted, [LBisection|Roads]) :-
	getBisector(O, LBisection),
	bisect(O, LBisection, MinArea, P1, P2), !,
	format('~w bisected into ~w and ~w', [O, P1, P2]), nl,
	classify_bisection(P1, MaxArea, Oversized1, Allotted1),
	merge(Oversized1, Os, Os1),
	classify_bisection(P2, MaxArea, Oversized2, Allotted2),
	merge(Oversized2, Os1, Os2),
	get_allotted(Allotted1, Allotted2, Allotted3),
	merge(Allotted3, AlRest, Allotted),
	generate_road_network(MinArea, MaxArea, Os2, AlRest, Roads).

classify_bisection(Area, Max, Area, []) :-
	area_size(Area, Size),
	Size >= Max.

classify_bisection(Area, Max, [], Area) :-
	area_size(Area, Size),
	Size < Max.

area_size(area(X1, X2, Y1, Y2), Size) :-
	area_size(X1, X2, Y1, Y2, Size).

area_size(X1, X2, Y1, Y2, Size) :-
	XSize is X2-X1,
	YSize is Y2-Y1,
	Size is XSize * YSize.

bisect(area(X1, X2, Y1, Y2), area(_, _, BY1, BY2), Min, area(X1, X2, Y1, BY1), area(X1, X2, BY2, Y2)) :-
	BY2 is BY1 + 1,
	area_size(X1, X2, Y1, BY1, Size1),
	area_size(X1, X2, BY2, Y2, Size2),
	Size1 > Min,
	Size2 > Min.

bisect(area(X1, X2, Y1, Y2), area(BX1, BX2, _, _), Min, area(X1, BX1, Y1, Y2), area(BX2, X2, Y1, Y2)) :-
	BX2 is BX1 + 1,
	area_size(X1, BX1, Y1, Y2, Size1),
	area_size(BX2, X2, Y1, Y2, Size2),
	Size1 > Min,
	Size2 > Min.

getBisector(area(X1, X2, Y1, Y2), area(X1, X2, BY1, BY2)) :-
	maybe, %% bisect it horizontally
	InnerY2 is Y2-1,
	random_between(Y1,InnerY2, BY1),
	BY2 is BY1 + 1.

getBisector(area(X1, X2, Y1, Y2), area(BX1, BX2, Y1, Y2)) :-	
	InnerX2 is X2-1,
	random_between(X1, InnerX2, BX1),
	BX2 is BX1 + 1.