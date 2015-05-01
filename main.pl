:- use_module(library(tcod)).
:- use_module(library(mavis)).
:- use_module(library(typedef)).
:- use_module(library(bearlibterminal)).

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
		Val is 1-(Radius**2.5),
		heightmap_set_value(Grad, X, Y, Val)
		;
		Val is 1-(Dist**2.5),
		heightmap_set_value(Grad, X, Y, Val)
	).

create_world :-
	W is 200,
	H is 50,
	create_heightmap(world, W, H),
	add_noise(world, 5, 5, 0, 0, 16, 0.5, 5),
	normalize(world, 0, 1),
	create_heightmap(gradient, W, H),
	foreach_heightmap(gradient, W, H, make_gradient(0.9)),
	create_heightmap(moisture, W, H),
	add_noise(moisture, 4, 4, 0, 0, 16, 0.5, 5),
	normalize(moisture, 0, 1),
	heightmap_multiply(gradient, world, world),
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
