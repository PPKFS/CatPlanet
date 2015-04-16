:- use_module(library(tcod)).
:- use_module(library(mavis)).
:- use_module(library(typedef)).


:- use_module(draw).
:- use_module(input).
:- use_module(utils).
:- use_module(citygen).
:- use_module(things).


main(150, 80, 0).
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
	%%draw_main, 
	input_main(Action),
	update_main(Action),
	run_loop.

%% run is det.
run:-
	init_main,
	run_loop.

centralise(Heightmap, W, H, X, Y) :-
	Rat is W/H,
	Dist is ((2*(X-(W/2))/(W-1)) ** 2) + ((2*(Y-(H/2))/(H-1)) ** 2),
	heightmap_get_value(Heightmap, X, Y, V),
	Thresh is Dist/2,
	(
		(V >= Thresh),
		NV is V * 3 * (2-Dist),
		heightmap_set_value(Heightmap, X, Y, NV)
		;
		V < Thresh,
		NV is V * 2 * (2-Dist),
		heightmap_set_value(Heightmap, X, Y, NV)
	).

create_world :-
	create_heightmap(world,  150, 80),
	add_noise(world, 3, 3, 0, 0, 16, 0.5, 10),
	scale_noise(world, 0.2, 0.2, 100, 0, 6, 0.5, 0.7),
	normalize(world, 0, 1),
	add_noise(world, 2, 2, 0, 0, 6, 0.5, 4),
	normalize(world, 0.3, 1.3),
	scale_noise(world, 2, 2, 0, 0, 6, 0.5, 0.5),
	normalize(world, 0, 1),
	%%heightmap_add_value(world, 0.4),
	%%normalize(world, 0, 1),
	foreach_heightmap(world, 150, 80, centralise),
	normalize(world, 0, 1),
	!,
	tcod:tcod_gen_map(8, [
		color(0, 0, 50), 
		color(30, 30, 170),
		color(114, 150, 71),
		color(80, 120, 10),
		color(17, 109, 7),
		color(120, 220, 120),
		color(208, 208, 239),
		color(255, 255, 255)],
		[0, 60, 68, 100, 140, 210, 220, 256], ColMap),
	foreach_heightmap(world, 150, 80, draw_heightmap(ColMap)),
	flush.

get_color(Val, ColMap, Color) :-
	nth0(Val, ColMap, Color).

draw_heightmap(Cols, Heightmap, W, H, X, Y) :-
	heightmap_get_value(Heightmap, X, Y, Val),
	Val2 is round(Val*255),
	get_color(Val2, Cols, Color), !,
	set_char_background(X, Y, Color).


%% go is det.
go :-
	%%things:init_database,
	console_size_full(main, W, H),
	console_size_full(command, MW, MH),
	console_size_full(info, IW, IH),
	console_size_full(map, MaW, MaH),
	init_root(W, H, 'Cat Planet', false),
	create_console(command, MW, MH),
	create_console(info, IW, IH),
	create_console(map, MaW, MaH),
	create_world.
	%%run.