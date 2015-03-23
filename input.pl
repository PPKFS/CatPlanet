:- module(input, 
	[
	read_word/2,
	input_main/2
	]).

:- use_module(library(console)).
:- use_module(library(events)).

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