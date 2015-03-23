:- module(utils,
	[
	console_size_full/3,
	console_size_full/4,
	console_size/3,
	console_size/4,
	remove_head/2
	]).

:- type number ---> float ; integer.

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

remove_head([_|T], T).