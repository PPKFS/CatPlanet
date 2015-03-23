:- use_module(library(console)).
:- use_module(library(color)).
:- use_module(library(events)).

:- use_module(draw).
:- use_module(input).
:- use_module(utils).

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