:- module(things,
	[
		init_database/0,
		load_fact/1,
		load_facts/1
	]).

:- use_module(library(persistency)).

:- use_module(utils).

:- persistent 
	active_object(id:atom),
	thing(id:atom, thing_type:atom),
	display_name(id:atom, name:atom),
	passageway_link(id:atom, fromID:atom, toID:atom).

init_database :-
	db_attach('db.foo', []),
	optionally(things:retractall_thing(_, _)),
	optionally(things:retractall_display_name(_, _)),
	optionally(things:retract_active_object(_)),
	optionally(things:retractall_passageway_link(_, _, _)),
	catch(db_sync(gc(50)), Catch, true), !.

load_fact(dig(Name)) :-
	load_fact(create(room, Name)).

load_fact(dig(Name, ID)) :-
	load_fact(create(room, Name, ID)).

load_fact(dig(Name, ID, Props)) :-
	load_fact(create(room, Name, ID, Props)).

load_fact(dig_passageway(Name, FromID, ToID, ID)) :-
	thing(FromID, room),
	thing(ToID, room),
	load_fact(create(passageway, Name, ID)),
	assert_passageway_link(ID, FromID, ToID).

load_fact(create(Type, Name)) :-
	load_fact(create(Type, Name, _, [])).

load_fact(create(Type, Name, ID)) :-
	load_fact(create(Type, Name, ID, [])).

load_fact(create(Type, Name, ID, Properties)) :-
	(
		nonvar(ID),
		(
			thing(ID, _),
			format('Cannot create a ~w called ~w, as the ID ~w is already in use.', [Type, Name, ID]),
			nl, !, fail
			;
			true
		)
		;
		var(ID),
		generate_ID(Name, ID)
	),
	assert_thing(ID, Type),
	assert_display_name(ID, Name),
	load_properties(ID, Properties),
	set_active_object(ID), !.

load_fact(description(Desc)) :-
	active_object(ID),
	load_fact(description(ID, Desc)).

load_fact(description(ID, Desc)):-
	(	
		description(ID, _),
		retract_description(ID, _)
		;
		\+ description(ID, _),
		true
	),
	assert_description(ID, Desc).

load_properties(_, _).

load_facts([]).

load_facts([T|Ts]) :-
	load_fact(T),
	load_facts(Ts).

load_facts_from_file(File) :-
	read_file_to_terms(File, Terms, []),
	load_facts(Terms).

set_active_object(ID) :-
	(
		retract_active_object(_)
		;
		true
	),
	assert_active_object(ID).

make_truncated_uuid(ID) :-
	uuid(UUID),
	atom_string(UUID, UUIDString),
	sub_string(UUIDString, _, 6, 0, ID).

generate_ID(Name, ID) :-
	get_time(Time),
	atomic_list_concat([_, Part|_], '.', Time),
	(
		sub_atom(Name, _, 8, _, Str), !
		;
		Str = Name
	),
	atom_concat(Str, Part, ID),
	\+ thing(ID, _).