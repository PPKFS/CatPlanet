:- use_module(library(console)).
:- use_module(library(color)).
:- use_module(library(events)).
:- use_module(library(mavis)).
:- use_module(library(typedef)).

:- use_module(draw).
:- use_module(input).
:- use_module(utils).
:- use_module(citygen).
:- use_module(things).


main(80, 60, 0).
command(60, 40, 1).
info(21, 40, 1).
map(80, 21, 1).

%% init_main is det.
init_main:-
	true.

%% update_main(Action) is det.
update_main(Action):-
	true.

%% run_loop is det.
run_loop:-
	draw_main,
	input_main(Action),
	update_main(Action),
	run_loop.

%% run is det.
run:-
	init_main,
	run_loop.

name_junction(point(X, Y), Name) :-
	atomic_list_concat([X, ' Street -', Y, ' Street Junction'], Name).

id_junction(point(X, Y), ID) :-
	atomic_list_concat(['Junction', X, '-', Y], ID).

make_junctions([], []).

make_junctions([LJ, RJ|Js], [LID, RID|IDs]) :-
	name_junction(LJ, LName),
	name_junction(RJ, RName),
	id_junction(LJ, LID),
	id_junction(RJ, RID),
	load_fact(dig(LName, LID)),
	load_fact(dig(RName, RID)),
	make_junctions(Js, IDs).

make_roads([], []).

make_roads([area(X1, X2, Y1, Y2)|Roads], [RID|RIDs]) :-
	1 is Y2-Y1,
	id_junction(point(X1, Y1), LeftID),
	id_junction(point(X2, Y1), RightID),
	load_fact(dig_passageway('Street', LeftID, RightID, RID)),
	make_roads(Roads, RIDs).

make_roads([area(X1, X2, Y1, Y2)|Roads], [RID|RIDs]) :-
	1 is X2-X1,
	id_junction(point(X1, Y1), LeftID),
	id_junction(point(X1, Y2), RightID),
	load_fact(dig_passageway('Street', LeftID, RightID, RID)),
	make_roads(Roads, RIDs).

%% test_city is det.
test_city :-
	things:init_database,
	generate_roads(area(0, 100, 0, 80), 6, 60, Areas, Roads, Junctions),
	make_junctions(Junctions, JIDs),
	make_roads(Roads, RIDs),
	init_root(100, 80, 'City Test', false),
	create_console(city, 100,  80),
	draw_roads(Roads),
	input_main(_, _).

%% draw_roads(List:list(area)) is det.
draw_roads([]) :-
	blit(city, 0, 0, 0, 0, root, 0, 0),
	flush.

draw_roads([area(X1, X2, Y1, Y2)|Rs]) :-
	color(lighter_green, Col1),
	color(darker_green, Col2),
	W is X2-X1,
	H is Y2-Y1,
	(
		H = 1,
		draw_line(city, h, X1, Y1, W, Col1)
		;
		draw_line(city, v, X1, Y1, H, Col2)
	),
	draw_roads(Rs).

create_city :-
	generate_roads(area(0, 100, 0, 80), 6, 60, Areas, Roads, Junctions),
	make_junctions(Junctions, JIDs),
	make_roads(Roads, RIDs).

%% go is det.
go :-
	things:init_database,
	console_size_full(main, W, H),
	console_size_full(command, MW, MH),
	console_size_full(info, IW, IH),
	console_size_full(map, MaW, MaH),
	init_root(W, H, 'Cat Planet', false),
	create_console(command, MW, MH),
	create_console(info, IW, IH),
	create_console(map, MaW, MaH),
	create_city,
	run.