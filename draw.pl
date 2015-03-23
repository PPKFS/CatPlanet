:- module(draw, 
	[
	draw_border/1,
	draw_main/1
	]).

:- use_module(library(console)).
:- use_module(library(color)).

draw_main(State):-
	console_size(main, W, H),
	console_size(command, CW, CH, CB),
	console_size(info, IW, IH),
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