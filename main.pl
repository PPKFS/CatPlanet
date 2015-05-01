:- use_module(library(tcod)).
:- use_module(library(mavis)).
:- use_module(library(typedef)).
:- use_module(library(bearlibterminal)).

:- use_module(draw).
:- use_module(input).
:- use_module(utils).
:- use_module(citygen).
:- use_module(things).


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
	Thresh is 2*Dist/3 - 0.3,
	(
		(V >= Thresh),
		NV is V * 5 * (2-Dist),
		heightmap_set_value(Heightmap, X, Y, NV)
		;
		V < Thresh,
		NV is V * 3 * (2-Dist),
		heightmap_set_value(Heightmap, X, Y, NV)
	).

generate_colormap(5) :-
	tcod:tcod_gen_map(15, [
		color(11, 10, 42), 
		color(7, 16, 59),
		color(7, 16, 59),
		color(0, 91, 10),
		color(8, 130, 151),
		color(10, 210, 198),
		color(219, 184, 130),
		color(248, 238, 202),
		color(225, 209, 132),
		color(197, 192, 111),
		color(161, 164, 77),
		color(153, 146, 78),
		color(118, 133, 78),
		color(61, 70, 41)],
		[0, 20, 30, 40, 55, 57, 58, 60, 62, 66, 70, 80, 100, 150, 256], ColMap).
generate_colormap(ColMap) :-
	tcod:tcod_gen_map(8, [
		color(0, 0, 50), 
		color(30, 30, 170),
		color(114, 150, 71),
		color(80, 120, 10),
		color(17, 109, 7),
		color(120, 220, 120),
		color(208, 208, 239),
		color(255, 255, 255)],
		[0, 60, 68, 100, 140, 210, 220, 256], ColMap).

make_gradient(Radius, Grad, W, H, X, Y) :-
	Nx is 2*X/W-1,
	Ny is 2*Y/H-1,
	Dist is sqrt((Nx**2)+(Ny**2)),
	(
		Dist > Radius,
		Val is 1-Radius,
		heightmap_set_value(Grad, X, Y, Val)
		;
		Val is 1-Dist,
		heightmap_set_value(Grad, X, Y, Val)
	).

create_world :-
	W is 200,
	H is 50,
	create_heightmap(world, W, H),
	add_noise(world, 3, 3, 0, 0, 16, 0.5, 10),
	scale_noise(world, 0.2, 0.2, 100, 0, 6, 0.5, 0.7),
	normalize(world, 0, 1),
	add_noise(world, 2, 2, 0, 0, 6, 0.5, 4),
	normalize(world, 0.3, 1.3),
	scale_noise(world, 2, 2, 0, 0, 6, 0.5, 0.5),
	normalize(world, 0, 1),
	foreach_heightmap(world, W, H, centralise),
	normalize(world, 0, 1),
	create_heightmap(gradient, W, H),
	foreach_heightmap(gradient, W, H, make_gradient(0.9)),
	create_heightmap(moisture, W, H),
	add_noise(moisture, 4, 4, 0, 0, 16, 0.5, 5),
	normalize(moisture, 0, 1),
	heightmap_multiply(gradient, moisture, world),
	create_heightmap(temperature, W, H),
	add_noise(temperature, 4, 4, 0, 0, 16, 0.5, 5),
	normalize(temperature, 0, 1),
	normalize(world, 0, 1),
	generate_colormap(ColMap),
	foreach_heightmap(world, W, H, draw_heightmap(temperature,moisture, ColMap)),
	terminal_refresh.

get_color(Val, ColMap, Color) :-
	nth0(Val, ColMap, Color).

get_color(Val2, _, _, Colmap, Color) :-
	Val2 < 68,
	nth0(Val2, Colmap, Color).

get_color(Val2, TV, MV, _, color(242, 222, 93)) :-
	TV >= 0.75,
	MV =< 0.2.
get_color(Val2, TV, MV, _, color(222, 242, 93)) :-
	TV >= 0.25,
	MV =< 0.25.
get_color(Val2, TV, MV, _, color(242, 244, 223)) :-
	TV =< 0.25,
	MV =< 0.5.
get_color(Val2, TV, MV, _, color(137, 141, 69)) :-
	TV > 0.75,
	MV < 0.5.
get_color(Val2, TV, MV, _, color(45,  142, 32)) :-
	TV > 0.5,
	MV < 0.5.
get_color(Val2, TV, MV, _, color(48, 63, 46)) :-
	TV =< 0.5,
	MV < 0.75.
get_color(Val2, TV, MV, _, color(24, 97, 15)) :-
	TV > 0.75,
	MV < 0.75.

get_color(Val2, TV, MV, _, color(59, 224, 38)) :-
	TV > 0.75,
	MV >= 0.75.

get_color(Val2, TV, MV, _, color(16, 105, 4)) :-
	TV > 0.5,
	MV >= 0.75.

get_color(Val2, TV, MV, _, color(86, 141, 79)).

draw_heightmap(Heightmap, W, H, X, Y) :-
	heightmap_get_value(Heightmap, X, Y, Val),
	Val2 is floor(Val*255),
	!,
	set_char_background(X, Y, color(Val2, Val2, Val2)).

draw_heightmap(Temp, Moisture, Colmap, Heightmap, W, H, X, Y) :-
	heightmap_get_value(Heightmap, X, Y, Val),
	Val2 is round(Val*255),
	heightmap_get_value(Temp, X, Y, TV),
	heightmap_get_value(Moisture, X, Y, MV),
	(
		Val2 > 130,
		TV2 is max(0, TV-(Val-0.35))
		;
		TV2 is TV
	),
	get_color(Val2, TV2, MV, Colmap, Color1),
	get_color(Val2, Colmap, Color2),
	lerp(Color1, Color2, 0.8, Color),
	!,
	terminal_bkcolor(Color),
	terminal_print(X, Y, " ").

%% go is det.
go :-
	terminal_open,
	terminal_set("window: size=200x50, cellsize=auto, title='Omni: Menu'; font=default"),
	terminal_color(color(255, 255, 255, 255)),
	terminal_clear,
	create_world,
	terminal_refresh.