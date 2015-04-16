:- module(utils,
	[
	console_size_full/3,
	console_size_full/4,
	console_size/3,
	console_size/4,
	remove_head/2,
	optionally/1,
	foreach_heightmap/4
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

optionally(X) :-
	call(X)
	;
	true.

foreach_heightmap(Heightmap, W, H, Do) :-
	foreach_heightmap(Heightmap, W, H, 0, 0, Do).

foreach_heightmap(_, W, H, X, Y, _) :-
	X is W-1,
	Y is H-1.

foreach_heightmap(Heightmap, W, H, W, Y, Do) :-
	Y1 is Y+1,
	foreach_heightmap(Heightmap, W, H, 0, Y1, Do).

foreach_heightmap(Heightmap, W, H, X, Y, Do) :-
	call(Do, Heightmap, W, H, X, Y),
	X1 is X+1,
	foreach_heightmap(Heightmap, W, H, X1, Y, Do).