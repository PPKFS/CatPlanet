:-module(classification, [class/1, subclass/2, thing/2]).

:-dynamic(thing/2).

class(thing).

subclass(_, thing).

subclass(abstract_thing, thing).
subclass(physical_thing, thing).

subclass(object, physical_thing).
subclass(process, physical_thing).

subclass(collection, abstract_thing).

subclass(region, object).

subclass(room, region).
subclass(passage, region).

subclass(attribute, abstract_thing).
subclass(relational_attribute, attribute).
subclass(positional_attribute, relational_attribute).
subclass(direction, positional_attribute).

subclass(X, X).

subclass(X, Y):-
	fact(subclass(X, Z)),
	subclass(Z, Y).