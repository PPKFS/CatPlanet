:- module(vocabulary, 
	[
		preposition/1,
		degree/1,
		noun/1,
		adjective/1,
		conjunction/1,
		sequence/1,
		determiner/1,
		applies_to/2,
		spatial_preposition/1,
		verb_extend/3,
		verb_synonym/2
	]).

:- dynamic(verb_synonym/2).

preposition(in).
preposition(on).
preposition(at).
spatial_preposition(on).

determiner(a).
determiner(the).

noun(cat).

adjective(red).

verb(look).
applies_to(look, nothing).
verb_extend(look, at, examine).

verb(examine).
applies_to(examine, one_thing).


degree(very).

sequence(then).

conjunction(and).