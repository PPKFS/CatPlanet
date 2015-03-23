:- use_module(library(console)).
:- use_module(library(color)).
:- use_module(library(events)).
:- use_module(library(mavis)).

:- use_module(draw).
:- use_module(input).
:- use_module(utils).
:- use_module(citygen).

main(80, 60, 0).
command(60, 40, 1).
info(21, 40, 1).
map(80, 21, 1).

init_main(State):-
	true.

update_main(Action, State, NewState):-
	true.

run_loop(State):-
	draw_main(State),
	input_main(State, Action),
	update_main(Action, State, State2),
	run_loop(State2).

run:-
	init_main(State),
	run_loop(State).

test_city :-
	generate_roads(area(0, 100, 0, 80), 15, 60, Areas, Roads),
	init_root(100, 80, 'City Test', false),
	create_console(city, 100,  80),
	draw_roads(Roads).

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
	draw_roads(Rs),
	input_main(_, _).

go :-
	console_size_full(main, W, H),
	console_size_full(command, MW, MH),
	console_size_full(info, IW, IH),
	console_size_full(map, MaW, MaH),
	init_root(W, H, 'Cat Planet', false),
	create_console(command, MW, MH),
	create_console(info, IW, IH),
	create_console(map, MaW, MaH),
	run.