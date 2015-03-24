:- module(verify, [load_game/2, load_world/3, clean_for_testing/0]).

:-use_module([library(uuid), assert_stuff]).

load_game([Rule|Rules], Success):-
	load_game([Rule|Rules], 1, Success).

load_game([Rule|Rules], LineNo, Success):-
	assert_game_rule(Rule, LineNo),
	NewLineNo is LineNo+1, !,
	load_game(Rules, NewLineNo, Success).

load_game([], _, true).

load_game(_, _, false).

load_world(File, Name, Success):-
	writef_line('Loading %w..', [Name]),
	read_file_to_terms(File, Terms, []),
	load_game(Terms, Success).
%
% Create objects/rooms
%

assert_game_rule(dig(Name), LN):-
	assert_game_rule(create(room, Name), LN).

assert_game_rule(dig(Name, ID), LN):-
	assert_game_rule(create(room, Name, ID), LN).

assert_game_rule(dig(Name, ID, Properties), LN):-
	assert_game_rule(create(room, Name, ID, Properties), LN).

assert_game_rule(create(Type, Name), LN):-
	assert_game_rule(create(Type, Name, _, []), LN).

assert_game_rule(create(Type, Name, ID), LN):-
	assert_game_rule(create(Type, Name, ID, []), LN).

assert_game_rule(create(Type, Name, ID, Properties), LN):-
	(
		nonvar(ID),
		(
			clause(things:thing(ID, _), _),
			writef('Line %w: Cannot create a %w called %w, as something with the ID %w exists..', [LN, Type, Name, ID]),
			nl, !, fail
			;
			true
		)
		;
		var(ID),
		generate_ID(Name, ID)
	),
	assert_thing(thing(ID, Type)),
	assert_thing(display_name(ID, Name)), !,
	assert_properties(ID, Properties, LN),
	set_active_object(ID).

% Description

assert_game_rule(description(Desc), LN):-
	get_active_object(Obj, LN),
	assert_game_rule(description(Obj, Desc), LN).

assert_game_rule(description(ID, Desc), LN):-
	assert_thing_overwrite(description(ID, Desc)).

assert_game_rule(exit(Direction, ToID, PassageID), LN):-
	get_active_object(Obj, LN),
	assert_game_rule(exit(Obj, Direction, ToID, PassageID), LN).

assert_game_rule(exit(FromID, Direction, ToID, PassageID), LN):-
	assert_game_rule(create(passage, 'Passageway', PassageID), LN).

% base
assert_game_rule(X, LN):-
	writef('Line %w: No idea what %w is!', [LN, X]), !, nl, fail.

assert_properties(_, [], _).
assert_properties(ID, [Prop|Props], LN):-
	Prop =.. [Property|Args],
	NewProp =.. [Property, ID|Args],
	assert_game_rule(NewProp, LN), !,
	assert_properties(ID, Props, LN).

set_active_object(ID):-
	(
		retract(things:active_object(_))
		;
		true
	),
	assert_thing(active_object(ID)).

get_active_object(ID, LN):-
	things:active_object(ID)
	;
	writef('Line %w: The active object was not set.', [LN]), nl, !, fail.

clean_for_testing:-
	assert_stuff:debug_log,
	retractall(things:thing(_,_)),
	retractall(things:display_name(_,_)),
	retractall(things:description(_, _)), !.