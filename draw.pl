:- module(draw, 
	[
	draw_border/1,
	draw_main/0,
	draw_line/6
	]).

:- use_module(library(console)).
:- use_module(library(color)).

draw_main:-
	console_size(command, _, CH, CB),
	console_size(info, IW, _),
	PromptY is CH-CB,
	putchar(command, CB, PromptY, '>'),
	draw_border(command),
	draw_border(info),
	draw_border(map),
	clear,
	blit(info, 0, 0, 0, 0, root, 0, 20),
	blit(command, 0, 0, 0, 0, root, IW, 20),
	blit(map, 0, 0, 0, 0, root, 0, 0),
	flush.

draw_line(Console, _, X, Y, 0, Col) :-
	set_char_background(Console, X, Y, Col).

draw_line(Console, v, X, Y, L, Col) :-
	set_char_background(Console, X, Y, Col),
	L1 is L-1,
	Y1 is Y+1,
	draw_line(Console, v, X, Y1, L1, Col).

draw_line(Console, h, X, Y, L, Col) :-
	set_char_background(Console, X, Y, Col),
	L1 is L-1,
	X1 is X+1,
	draw_line(Console, h, X1, Y, L1, Col).

draw_border(Console, 0, 0, Col) :-
	set_char_background(Console, 0, 0, Col).

draw_border(Console, W, 0, Col) :-
	console_size(Console, _, H),
	set_char_background(Console, W, 0, Col),
	set_char_background(Console, W, H, Col),
	W1 is W-1,
	draw_border(Console, W1, 0, Col).

draw_border(Console, W, H, Col) :-
	set_char_background(Console, W, H, Col),
	set_char_background(Console, 0, H, Col),
	H1 is H-1,
	draw_border(Console, W, H1, Col).

draw_border(Console) :-
	console_size(Console, _, _, B),
	B = 0.

draw_border(Console) :-
	console_size(Console, W, H),
	color(lighter_green, Col),
	draw_border(Console, W, H, Col).