%%parser.pl - parses commands.

:- module(parser, [parse/2]). 

:- use_module(vocabulary).

%
% Top-level parsing handler
%
parse(Input, Command):-
	parse(Structure, Input, []),
	parse_action(Structure, Command),
	match_action(Command).

parse(Tree) -->
	command_phrase_rule(Tree).

command_phrase_rule(Tree) -->
	verb_phrase_rule(Tree).

command_phrase_rule([Verb_Phrase|Verb_Phrases]) -->
	verb_phrase_rule(Verb_Phrase),
	sequence_rule(_),
	command_phrase_rule(Verb_Phrases).

%
% Verb Phrases
%
verb_phrase_rule(parse_tree(Verb_Phrase)) -->
	verb_rule(Verb_Phrase).

% e.g. look up. go out.
verb_phrase_rule(parse_tree(Verb_Phrase, Preposition)) -->
	verb_rule(Verb_Phrase),
	preposition_rule(Preposition).

%Needs to be first to avoid it getting eaten wrongly. 'look at the stars with the telescope'.
%the with is implicit - it'll parse to a command of the type (verb-obj1-obj2).
verb_phrase_rule(parse_tree(Verb_Phrase, Noun_Phrase, P1,  NP2)) -->
	verb_rule(Verb_Phrase),
	prepositional_phrase_rule(prepositional_phrase(P1, Noun_Phrase)),
	prepositional_phrase_rule(prepositional_phrase(P2, NP2)),
	{P2 = preposition(with)}.

%look at the floor.
verb_phrase_rule(parse_tree(Verb_Phrase, P, NP)) -->
	verb_rule(Verb_Phrase),
	prepositional_phrase_rule(prepositional_phrase(P, NP)).

%% throw the brick at the door.
verb_phrase_rule(parse_tree(Verb_Phrase, Noun_Phrase, P, NP)) -->
	verb_rule(Verb_Phrase),
	noun_phrase_rule(Noun_Phrase),
	prepositional_phrase_rule(prepositional_phrase(P, NP)).

% e.g. throw brick. throw the brick.
verb_phrase_rule(parse_tree(Verb_Phrase, Noun_Phrase)) -->
	verb_rule(Verb_Phrase),
	noun_phrase_rule(Noun_Phrase).

%
% Noun/Adjective/Prepositional Phrases
%

noun_phrase_rule(noun_phrase(Determiner, Noun_Phrase)) -->
	determiner_rule(Determiner),
	adjectival_noun_phrase_rule(Noun_Phrase).

noun_phrase_rule(noun_phrase(Determiner, Noun_Phrase, Preposition)) -->
	determiner_rule(Determiner),
	adjectival_noun_phrase_rule(Noun_Phrase),
	prepositional_phrase_rule(Preposition),
	{Preposition \= prepositional_phrase('[]')}.

adjectival_noun_phrase_rule(noun(N)) -->
	noun_rule(noun(N)).
adjectival_noun_phrase_rule(noun(Adjective, Noun)) -->
	adjectival_phrase_rule(Adjective),
	adjectival_noun_phrase_rule(noun(Noun)).

adjectival_phrase_rule(adjective(Degree, Adjective)) -->
	degree_rule(Degree),
	adjective_rule(adjective(Adjective)).

prepositional_phrase_rule(prepositional_phrase(preposition(Preposition), Noun_Phrase)) -->
	preposition_rule(preposition(Preposition)),
	noun_phrase_rule(Noun_Phrase).
prepositional_phrase_rule(prepositional_phrase('[]')) --> [].

%
% Terminals and other parts
%
terminal(Input, Term) --> [Input], { call(vocabulary:Term, Input) }.
terminal(Input, Term, Match) --> [Input], { call(vocabulary:Term, Input, Match) }.
optional_terminal(Input, Term) --> terminal(Input, Term) ; [], {Input = '[]'}.

degree_rule(degree(D)) --> optional_terminal(D, degree).
adjective_rule(adjective(A)) --> optional_terminal(A, adjective).

determiner_rule(determiner(D)) --> optional_terminal(D, determiner).
noun_rule(noun(N)) --> terminal(N, noun).
preposition_rule(preposition(P)) --> terminal(P, preposition).
sequence_rule(sequence(S)) --> terminal(S, sequence).
sequence_rule(sequence(S)) --> terminal(S, conjunction).

verb_rule(verb(V)) --> terminal(V, verb).
%%verb_rule(verb(V)) --> [V2], {verb_synonym(V2, V), verb(V)}.

%
% Action parsing
%

parse_action([], []).
parse_action([Action_Phrase|Action_Phrases], [Action|Actions]):-
	parse_action(Action_Phrase, Action),
	parse_action(Action_Phrases, Actions).

parse_action(parse_tree(verb(Verb)), action(player, Verb)).
parse_action(parse_tree(verb(Verb), Noun_Phrase), call_action(player, Verb, Noun_Phrase)).

parse_action(parse_tree(verb(Verb), preposition(Preposition)), action(player, New_Action)):-
	translate_action(Verb, Preposition, New_Action).

%if the preposition is spatial (e.g. above), then consider it as part of the phrase. If it's not (e.g. at), then consider it part of the verb.
% note that 'in' and 'on' aren't spatial since they're special (they convert to insert and place_on as they only apply to containers and supportable objects)
% also on can be both 'put the ball on the chair' and 'use the key on the door'.
parse_action(parse_tree(verb(Verb), preposition(P), Noun_Phrase), action(player, Verb, preposition(P), Noun_Phrase)):-
	spatial_preposition(P).

parse_action(parse_tree(verb(Verb), preposition(P), Noun_Phrase), action(player, New_Action, Noun_Phrase)):-
	translate_action(Verb, P, New_Action).

%same as above, except for 'use key on the door'
parse_action(parse_tree(verb(Verb), Noun_Phrase, preposition(Preposition), Noun_Phrase2), action(player, Verb, preposition(Preposition), Noun_Phrase, Noun_Phrase2)):-
	spatial_preposition(Preposition).

parse_action(parse_tree(verb(Verb), Noun_Phrase, preposition(Preposition), Noun_Phrase2), action(player, New_Action, Noun_Phrase, Noun_Phrase2)):-
	translate_action(Verb, Preposition, New_Action).

% Unneeded?
translate_action(Verb, Preposition, New_Verb):-
	verb_extend(Verb, Preposition, New_Verb).

translate_action(Verb, Preposition, New_Verb):-
	verb_synonym(Verb, Verb2),
	verb_extend(Verb2, Preposition, New_Verb).

%
% Verb Matching
%

match_action(action(_, Verb)):-
	applies_to(Verb, nothing).

match_action(action(_, Verb, _)):-
	applies_to(Verb, one_thing).

match_action(action(_, Verb, preposition(_), _)):-
	applies_to(Verb, one_thing).

match_action(action(_, Verb, preposition(_), _, _)):-
	applies_to(Verb, two_things).

match_action(action(_, Verb, _, _)):-
	applies_to(Verb, two_things).