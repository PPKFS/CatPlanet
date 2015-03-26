
%% based off 'An Iterated Subdivision Algorithm For Procedural Road Plan Generation' by Rudzicz and Verbrugge
:- module(citygen,
	[
		generate_roads/6
	]).

:- use_module(library(mavis)).
:- use_module(library(typedef)).

:- use_module(utils).

:- type area ---> area(integer, integer, integer, integer).
:- type area_or_empty ---> area(integer, integer, integer, integer) ; [].
:- type point ---> point(integer, integer).

disable_mavis.

%% get_allotted(X, Y, Z:list) is det.
get_allotted([], [], []).
get_allotted([], Y, [Y]).
get_allotted(X, [], [X]).
get_allotted(X, Y, [X, Y]).

%% empty(X:list) is semidet.
empty([]).

%% generate_roads(CitySize:area, MinArea:positive_integer, MaxArea:positive_integer, Allotted:list(area), 
%% Roads:list(area), Junctions:list(point)) is det.
generate_roads(CitySize, MinArea, MaxArea, Allotted, Roads, Junctions) :-
	MaxArea < 4,
	write('Max area must be greater than 3 (else 2x2 squares cannot be further subdivided).'), nl, !, fail
	;
	generate_road_network(MinArea, MaxArea, [CitySize], AllottedUnfiltered, Roads, Junctions),
	exclude(empty, AllottedUnfiltered, Allotted),
	length(Allotted, LA),
	length(Roads, LR),
	length(Junctions, LJ),
	format('Generated ~w areas and ~w roads and ~w junctions', [LA, LR, LJ]), !.

%% generate_road_network(Min:positive_integer, MaxArea:positive_integer, 
%% Oversized:list(area_or_empty), Allotted:list(area_or_empty), Roads:list(area), Junctions:list(point)) is multi.
generate_road_network(_, _, [], [], [], []) :-
	write('Done!').

generate_road_network(MinArea, MaxArea, [[]|Os], As, Roads, Junctions) :-
	generate_road_network(MinArea, MaxArea, Os, As, Roads, Junctions).

generate_road_network(MinArea, MaxArea, [O|Os], [A1, A2|As], [LBisection|Roads], [LJ, RJ|Junctions]) :-
	get_bisector(O, LBisection),
	bisect(O, LBisection, MinArea, 0.15, 10, P1, P2), !,
	%%format('~w bisected into ~w and ~w', [O, P1, P2]), nl,
	classify_bisection(P1, MaxArea, Oversized1, A1),
	classify_bisection(P2, MaxArea, Oversized2, A2), !,
	build_junctions(LBisection, LJ, RJ),
	generate_road_network(MinArea, MaxArea, [Oversized1, Oversized2|Os], As, Roads, Junctions).

%% classify_bisection(Area:area, Max:positive_integer, Oversized:area_or_empty, Area:area_or_empty) is det.
classify_bisection(Area, Max, [], Area) :-
	area_size(Area, Size),
	Size =< Max.

classify_bisection(Area, Max, Area, []) :-
	area_size(Area, Size),
	Size > Max.

build_junctions(area(X1, X2, Y1, Y2), point(X1, Y1), point(X2, Y1)) :-
	1 is Y2-Y1. %% horizontal road

build_junctions(area(X1, X2, Y1, Y2), point(X1, Y1), point(X1, Y2)) :-
	1 is X2-X1.

%% area_size(Area:area, Size:integer) is det.

area_size(area(X1, X2, Y1, Y2), Size) :-
	area_size(X1, X2, Y1, Y2, Size).

%% area_size(X1:integer, X2:integer, Y1:integer, Y2:integer, Size:integer) is det.
area_size(X1, X2, Y1, Y2, Size) :-
	XSize is X2-X1,
	YSize is Y2-Y1,
	Size is XSize * YSize.

%% area_ratio(X1:integer, X2:integer, Y1:integer, Y2:integer, Ratio:number) is det.
area_ratio(X1, X2, Y1, Y2, Ratio) :-
	XSize is X2-X1,
	YSize is Y2-Y1,
	Ratio is XSize / YSize.

%% bisect(Area:area, Bisection:area, MinSize:positive_integer, MinRat:float, MaxRat:number, P1:area, P2:area) is semidet.
bisect(area(X1, X2, Y1, Y2), area(_, _, BY1, BY2), Min, _, MaxRat, area(X1, X2, Y1, BY1), area(X1, X2, BY2, Y2)) :-
	BY2 is BY1 + 1,
	area_size(X1, X2, Y1, BY1, Size1),
	area_size(X1, X2, BY2, Y2, Size2),
	area_ratio(X1, X2, Y1, BY1, Ratio1),
	area_ratio(X1, X2, BY2, Y2, Ratio2),
	Ratio1 < MaxRat,
	Ratio2 < MaxRat,
	Size1 >= Min,
	Size2 >= Min.

bisect(area(X1, X2, Y1, Y2), area(BX1, BX2, _, _), Min, MinRat, _, area(X1, BX1, Y1, Y2), area(BX2, X2, Y1, Y2)) :-
	BX2 is BX1 + 1,
	area_size(X1, BX1, Y1, Y2, Size1),
	area_size(BX2, X2, Y1, Y2, Size2),
	area_ratio(X1, BX1, Y1, Y2, Ratio1),
	area_ratio(BX2, X2, Y1, Y2, Ratio2),
	Ratio1 > MinRat,
	Ratio2 > MinRat,
	Size1 >= Min,
	Size2 >= Min.

%% get_bisector(A1:area, A2:area) is multi.
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