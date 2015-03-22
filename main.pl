:- use_module(library(console)).
:- use_module(library(color)).
:- use_module(library(events)).

remove_head([_|T], T).

main(80, 60, 0).
command(60, 40, 1).
info(21, 40, 1).
map(80, 21, 1).

console_size_full(Console, W, H) :-
	call(Console, W, H, _).

console_size_full(Console, W, H, B) :-
	call(Console, W, H, B).

console_size(Console, W, H) :-
	call(Console, W1, H1, _),
	W is W1-1,
	H is H1-1.

console_size(Console, W, H, B) :-
	call(Console, W1, H1, B),
	W is W1-1,
	H is H1-1.

init_main(State):-
	true.

update_main(Action, State, NewState):-
	true.

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

read_char(Char):-
	repeat,
	(
		check_for_keypress(Key),
		key_to_char(Key, Char)
		;
		window_is_closed, !, nl,
		halt
		;
		fail
	).

write_input_to_console(CharList):-	
	reverse(CharList, X),
	atom_chars(Str, X),
	console_size(command, _, H),
	H1 is H-1,
	print(command, 2, H1, Str),
	draw_main(_).

read_word(Chars, Word):-
	read_char(Char),
	(
		Char = '\n',
		reverse(Chars, CharList),
		atom_chars(Word, CharList)
		;
		Char = '\b',
		remove_head(Chars, Backspaced),
		write_input_to_console([' '|Backspaced]),
		read_word(Backspaced, Word)
		;
		NewStr = [Char|Chars],
		write_input_to_console(NewStr),
		read_word(NewStr, Word)
	).

input_main(State, Action):-
	read_word([], Word),
	write(Word), nl.

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