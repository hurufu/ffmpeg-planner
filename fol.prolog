:- module(fol, [pnf_pddl/2]).

:- set_prolog_flag(double_quotes, chars).
:- initialization(all_tests_are_passing).

:- use_module(library(dif)).

main(AcePnfFile) :-
    consult(AcePnfFile),
    pnf(AcePnf),
    pnf_pddl(AcePnf, ProblemDefinition),
    write(ProblemDefinition).

pnf_test(ProblemDefinition) :-
    tt(TestPnf),
    pnf_pddl(TestPnf, ProblemDefinition).

pnf_pddl(AcePnf, ProblemDefinition) :-
    compact(AcePnf, Formula, ObjectList),
    phrase(pddl(ObjectList,Formula), PddlChars),
   %phrase(objects(ObjectList), PddlChars),
   %phrase(goal(Formula), PddlChars),
    atom_chars(ProblemDefinition, PddlChars).

% Unifies APE FOL formula with a compact one which can be easily converted to
% PDDL FOL formula and extracted list of proper names which should go to (:objects)
% TODO: Handle arbitrary FOL formula, not just PNF
% Unifies FOL formula with an equivalent one with all similar operators condensed
% Extract all named entities to :objects
% Unifies FOL formula in S-expr format with equivalent one where all 'be'
% predicates are replaced with '='
compact(FolFormula, CompactFolFormula, ContainedProperNames) :-
    must_not_exist(compa(_,_)),
    sanitize(FolFormula, SanitizedFolFormula, ContainedProperNames),
    findall(X, g(X), Xs),
    setup_call_cleanup(
        maplist(assertz, Xs),
        compa(SanitizedFolFormula, CompactFolFormula),
        abolish(compa/2)
    ).

g((
    compa(U, U)
)) :-
    l(U).

g((
    compa(-(A), n(AX)) :-
        compa(A, AX)
)).

g((
    compa(U, UX) :-
        compa(A, AX),
        compa(B, BX),
        join(Class, AX, BX, X)
)) :-
    univ_a(U, Op, [A,B]),
    map_op(Op, MappedOp),
    UX =.. [MappedOp,X],
    r(Op, Class, AX, BX).

g((
    compa(U, UX) :-
        compa(B, BX)
)) :-
    quantifier(OpA, OpX),
    qx(OpX, BX),
    U =.. [OpA,A,B],
    UX =.. [OpX,[A],BX].

g((
    compa(U, UX) :-
        compa(B, UY)
)) :-
    quantifier(OpA, OpX),
    U =.. [OpA,A,B],
    UX =.. [OpX,[A|BV],BX],
    UY =.. [OpX,BV,BX].

join(Class, A, B, J) :-
    must_succeed(join_aux(Class,A,B,J)).
join_aux( both, A, B, J) :- A =.. [_,F], B =.. [_,G], append([ F , G ], J).
join_aux( left, A, B, J) :- A =.. [_,F],              append([ F ,[B]], J).
join_aux(right, A, B, J) :- B =.. [_,G],              append([[A], G ], J).
join_aux( none, A, B, J) :-                           append([[A],[B]], J).

r(UpperOp, Class, A, B) :-
    map_op(UpperOp, Op),
    q(A),
    q(B),
    A =.. [AH|_],
    B =.. [BH|_],
    classify(Op, Class, AH, BH).

classify(a, none, b, b).
classify(a, none, b, p).
classify(a, right, b, a).
classify(a, none, b, o).
classify(a, none, b, n).
classify(a, none, p, b).
classify(a, none, p, p).
classify(a, right, p, a).
classify(a, none, p, o).
classify(a, none, p, n).
classify(a, left, a, b).
classify(a, left, a, p).
classify(a, both, a, a).
classify(a, left, a, o).
classify(a, left, a, n).
classify(a, none, o, b).
classify(a, none, o, p).
classify(a, right, o, a).
classify(a, none, o, o).
classify(a, none, o, n).
classify(a, none, n, b).
classify(a, none, n, p).
classify(a, right, n, a).
classify(a, none, n, o).
classify(a, none, n, n).
classify(o, none, b, b).
classify(o, none, b, p).
classify(o, none, b, a).
classify(o, right, b, o).
classify(o, none, b, n).
classify(o, none, p, b).
classify(o, none, p, p).
classify(o, none, p, a).
classify(o, right, p, o).
classify(o, none, p, n).
classify(o, none, a, b).
classify(o, none, a, p).
classify(o, none, a, a).
classify(o, right, a, o).
classify(o, none, a, n).
classify(o, left, o, b).
classify(o, left, o, p).
classify(o, left, o, a).
classify(o, both, o, o).
classify(o, left, o, n).
classify(o, none, n, b).
classify(o, none, n, p).
classify(o, nonet, n, a).
classify(o, right, n, o).
classify(o, none, n, n).

l(b(_,_)).
l(p(_,_,_)).
q(a(_)).
q(o(_)).
q(n(_)).
q(X) :- l(X).
qx(f, e(_,_)).
qx(e, f(_,_)).
qx(f, X) :- q(X).
qx(_, X) :- q(X).

map_op(&, a).
map_op(v, o).

quantifier(exists, e).
quantifier(forall, f).

univ_a(&(A,B), &, [A,B]).
univ_a(v(A,B), v, [A,B]).

must_succeed(Goal) :- call(Goal), !.
must_succeed(Goal) :- throw(error(must_succeed,Goal)).

% Sanitizes APE PNF formula and extracts all proper names from it.
%
% For a sanitized formula the following is true:
%
%   * It has all unused parameters removed from logical atoms
%   * All references are tagged `i/1` and are set to be different from one another
%   * 'be' is replaced with '='
%   * An existence of a default world is added
%
% TODO: Check if it handles properly any FOL formula and not just PNF.
% TODO: Handle verb 'be' better, more info in ACE specification.
% FIXME: Accepts only single world from possible worlds semantics.
% WARNING: This predicate isn't monotonic, because it is impossible to make it
%          such. Input FOL doesn't destinguish between references and free
%          variables that's why I have to assume that all free variables must
%          be references.
sanitize(FolFormula, SanitizedFolFormula, ProperNamesWithoutDuplicates) :-
    sanitize_aux(exists(World, FolFormula), World, SanitizedFolFormula, ProperNames, References),
    all_different(References),
    sort(ProperNames, ProperNamesWithoutDuplicates).
sanitize_aux(A, W, X, O, []) :-
    compact_edge(A, W, X, O).
sanitize_aux(Q, W, QX, O, R) :-
    univ_quantifier(Q, Op, [A,B]),
    univ_quantifier(QX, Op, [i(A),BX]),
    must_be_variable(A),
    sanitize_aux(B, W, BX, O, BR),
    append([[i(A)],BR], R).
sanitize_aux(Q, W, QX, O, R) :-
    univ_binary_operator(Q, Op, [A,B]),
    univ_binary_operator(QX, Op, [AX,BX]),
    sanitize_aux(A, W, AX, AO, AR),
    sanitize_aux(B, W, BX, BO, BR),
    append([AO,BO], O),
    append([AR,BR], R).
sanitize_aux(-(A), W, -(AX), O, R) :-
    sanitize_aux(A, W, AX, O, R).

univ_binary_operator(&(A,B), &, [A,B]).
univ_binary_operator(v(A,B), v, [A,B]).
univ_quantifier(exists(A,B), exists, [A,B]).
univ_quantifier(forall(A,B), forall, [A,B]).

% Description of each argument can be found in DRS report 6.7
compact_edge(EdgeTerm, World, SimplifiedEdgeTerm, ContainedProperNames) :-
    EdgeTerm = object(World, Ref, Noun, _Class, _Unit, _Op, _Count)-_Index,
    SimplifiedEdgeTerm = b(Noun, TaggedRef),
    tag(Ref, TaggedRef, ContainedProperNames).
compact_edge(EdgeTerm, World, SimplifiedEdgeTerm, ContainedProperNames) :-
    EdgeTerm = predicate(World, _Ref, Verb, SubjRef, ObjRef)-_Index,
    SimplifiedEdgeTerm = p(MappedVerb, TaggedSubjRef, TaggedObjRef),
    verb_mapping(Verb, MappedVerb),
    tag(SubjRef, TaggedSubjRef, O1),
    tag(ObjRef, TaggedObjRef, O2),
    append([O1,O2], ContainedProperNames).

% Verb 'be' is somewhat special in ACE - it has predefined meaning which can be
% conviniently mapped to equality in PDDL.
% FIXME: Actually in ACE this verb is far more overloaded, so only this simple
%        use is supported.
verb_mapping(be, =).
verb_mapping(V, V) :- dif(V, be), dif(V, =).

% Validates edge terms and assigns tags to free variables
% FIXME: This predicate isn't monotonic. Is there a better way to do it?
% -- There isn't because there is no destinction between References and free
%    variables in the PNF output
tag(A, X, O) :-
(
    ground(A) ->
    (
        tag_term(A, X, O) ->
            true;
            throw(domain_error(named|string, A))
    );
    (
        var(A) ->
            tag_variable(A, X, O);
            throw(type_error(ground|var, A))
    )
).
tag_variable(A, i(A), []).
tag_term(named(A), n(A), [n(A)]).
tag_term(string(A), s(A), [s(A)]).

must_be_variable(A) :- nonvar(A) -> throw(type_error(var,A)); true.

all_different([]).
all_different([H|T]) :-
    maplist(dif(H), T),
    all_different(T).

pddl(O,G) --> "(define (problem test)", n,
              "        (:domain ffmpeg)", n,
              "        (:requirements :equality)", n,
              "        ", objects(O), n,
              "        (:init ", predefined_init, ")", n,
              "        ", goal(G), ")", n.
n --> "\n".
objects(V) --> "(:objects", sequence_of_objects_spaced(V), predefined_objects, ")".

predefined_objects --> " f0 sample_mp4 s00 s01".
predefined_init --> "(input-file f0)", n,
     "               (name sample_mp4)", n,
     "               (taken sample_mp4)", n,
     "               (have f0 sample_mp4)", n,
     "               (stream s00)", n,
     "               (audio-stream s00)", n,
     "               (stream s01)", n,
     "               (video-stream s01)", n,
     "               (have f0 s00)", n,
     "               (have f0 s01)", n,
     "               (encode Aac s00)", n,
     "               (encode Libx264 s01)", n,
                     timeless.
timeless -->
     "               (codec Libx264)", n,
     "               (codec Libx265)", n,
     "               (codec Aac)", n,
     "               (codec Libopus)", n,
     "               (video-codec Libx264)", n,
     "               (video-codec Libx265)", n,
     "               (audio-codec Aac)", n,
     "               (audio-codec Libopus)".
sequence_of_objects_spaced([]) --> "".
sequence_of_objects_spaced([O|T]) --> identifier(O), sequence_of_objects_spaced(T).

goal(G) --> "(:goal ", ex(G), ")".

ex(ax([])) --> "".
ex(ox([])) --> "".
ex(b(O,A)) --> { atom_chars(O,OC) }, "(", OC, identifier(A), ")".
ex(p(P,A,B)) --> { atom_chars(P, PC) }, "(", PC, identifier(A), identifier(B), ")".
ex(n(X)) --> "(not ", ex(X), ")".
ex(e(H, X)) --> "(exists", list_of_variables(H), " ", ex(X), ")".
ex(f(H, X)) --> "(forall", list_of_variables(H), " ", ex(X), ")".
ex(a([H|T])) --> "(and ", ex(H), " ", ex(ax(T)), ")".
ex(o([H|T])) --> "(or ", ex(H), " ", ex(ox(T)), ")".
ex(ax([H|T])) --> ex(H), " ", ex(ax(T)).
ex(ox([H|T])) --> ex(H), " ", ex(ox(T)).

identifier(i(V)) --> " ", variable(V).
identifier(n(V)) --> " ", { atom_chars(V, VC) }, VC.
identifier(s(V)) --> " ", { filename_adjusted(V, VC) }, VC.

list_of_variables([i(H)|T]) --> " (", variable(H), list_of_variables_spaced(T), ")".
list_of_variables_spaced([]) --> "".
list_of_variables_spaced([i(H)|T]) --> " ", variable(H), list_of_variables_spaced(T).

variable(V) --> "?", alpha(A), alpha(B), { atom_chars(V, [A,B]) }.

alnum(C) --> char(alnum, C).
numeric(C) --> char(numer, C).
alpha(C) --> char(alpha, C).
char(Class, C) --> [C], { ychar(Class, C) }.
ychar(numer, C) :- xchar(1,_,_, C).
ychar(alpha, C) :- xchar(_,1,_, C).
ychar(alnum, C) :- xchar(_,_,1, C).
xchar(1,0,1,'0').
xchar(1,0,1,'1').
xchar(1,0,1,'2').
xchar(1,0,1,'3').
xchar(1,0,1,'4').
xchar(1,0,1,'5').
xchar(1,0,1,'6').
xchar(1,0,1,'7').
xchar(1,0,1,'8').
xchar(1,0,1,'9').
xchar(0,1,1,'a').
xchar(0,1,1,'b').
xchar(0,1,1,'c').
xchar(0,1,1,'d').
xchar(0,1,1,'e').
xchar(0,1,1,'f').
xchar(0,1,1,'g').
xchar(0,1,1,'h').
xchar(0,1,1,'i').
xchar(0,1,1,'j').
xchar(0,1,1,'k').
xchar(0,1,1,'l').
xchar(0,1,1,'m').
xchar(0,1,1,'n').
xchar(0,1,1,'o').
xchar(0,1,1,'p').
xchar(0,1,1,'q').
xchar(0,1,1,'r').
xchar(0,1,1,'s').
xchar(0,1,1,'t').
xchar(0,1,1,'u').
xchar(0,1,1,'v').
xchar(0,1,1,'w').
xchar(0,1,1,'x').
xchar(0,1,1,'y').
xchar(0,1,1,'z').

% TODO: Remove duplicated predicates
% Workaround for not supported strings in PDDL
% Replaces last "_" with "."
filename_adjusted(GoodFileNameAtom, PddlFileNameChars) :-
    atom_chars(GoodFileNameAtom, GoodFileNameChars),
    phrase(adjust(GoodFileNameChars), PddlFileNameChars), !.

adjust(X) --> dirname(D), basename(B), "_", suffix(S), { append([D,B,".",S], X) }.
dirname([]) --> "". % Not implemented.
basename([]) --> "".
basename([H|T]) --> [H], basename(T).
suffix("mp4") --> "mp4".
suffix("mp3") --> "mp3".
suffix("mkv") --> "mkv".
suffix("avi") --> "avi".
suffix("ogg") --> "ogg".
suffix("webm") --> "webm".
suffix("webp") --> "webp".


any_sequence([]) --> [].
any_sequence([H|T]) --> [H], any_sequence(T).

any_sequence_of_any_sequences([]) --> [].
any_sequence_of_any_sequences([H|T]) --> any_sequence(H), any_sequence_of_any_sequences(T).

% TODO: Should throw an exception is any of parameters is not a list
concatenation(ListOfLists, Concatenation) :-
    phrase(any_sequence_of_any_sequences(ListOfLists), Concatenation).

% UT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(S, P, T, Y) :-
    S = 'p:f0 is a file which has a name which is "hello.mkv"',
    P = exists(A,exists(B,exists(C,exists(D,exists(E,
        &(
            object(F,A,file,countable,na,eq,1)-1/6,
            &(
                object(F,B,name,countable,na,eq,1)-1/10,
                &(
                    predicate(F,C,be,B,string('hello.mkv'))-1/12,
                    &(
                        predicate(F,D,have,A,B)-1/8,
                        predicate(F,E,be,named(f0),A)-1/4))))))))),
    T = [exists, [A, B, C, D, E],
                 [and, [file, A],
                       [name, B],
                       [=, B, 'hello_mkv'],
                       [have, A, B],
                       [=, f0, A]]],
    Y = e([i(A),i(B),i(C),i(D),i(E)],
          a([o(file, i(A)),
             o(name, i(B)),
             p(=, i(B), s('hello_mkv')),
             p(have, i(A), i(B)),
             p(=, n(f0), i(A))])).

tt(exists(_34206,exists(_34238,exists(_34270,exists(_34302,exists(_34334,exists(_34366,exists(_34398,exists(_34430,exists(_34462,exists(_34494,exists(_34526,exists(_34558,exists(_34590,exists(_34622,exists(_34654,exists(_34686,exists(_34718,exists(_34750,exists(_34782,forall(_39780,forall(_39812,forall(_40446,forall(_40478,forall(_41112,forall(_41144,forall(_41778,forall(_41810,forall(_42444,forall(_42476,forall(_43110,forall(_43142,forall(_43776,forall(_43808,forall(_44442,forall(_44474,forall(_45108,forall(_45140,&(object(_107538,_34206,name,countable,na,eq,1)-1,&(predicate(_107538,_34238,be,string('sample.mkv'),_34206)-1,&(object(_107538,_34270,file,countable,na,eq,1)-1,&(predicate(_107538,_34302,be,named(f1),_34270)-1,&(object(_107538,_34334,stream,countable,na,eq,1)-1,&(predicate(_107538,_34366,be,named(s11),_34334)-1,&(object(_107538,_34398,stream,countable,na,eq,1)-1,&(predicate(_107538,_34430,be,named(s10),_34398)-1,&(object(_107538,_34462,'audio-stream',countable,na,eq,1)-1,&(predicate(_107538,_34494,be,named(s11),_34462)-1,&(object(_107538,_34526,'video-stream',countable,na,eq,1)-1,&(predicate(_107538,_34558,be,named(s10),_34526)-1,&(object(_107538,_34590,'output-file',countable,na,eq,1)-1,&(predicate(_107538,_34622,be,named(f1),_34590)-1,&(predicate(_107538,_34654,have,named(f1),string('sample.mkv'))-1,&(predicate(_107538,_34686,have,named(f1),named(s11))-1,&(predicate(_107538,_34718,have,named(f1),named(s10))-1,&(predicate(_107538,_34750,encode,named('Libx265'),named(s10))-1,&(predicate(_107538,_34782,encode,named('Libopus'),named(s11))-1,&(v(- (object(_107538,_39780,codec,countable,na,eq,1)-1),- (predicate(_107538,_39812,be,named(f1),_39780)-1)),&(v(- (object(_107538,_40446,stream,countable,na,eq,1)-1),- (predicate(_107538,_40478,be,named(f1),_40446)-1)),&(v(- (object(_107538,_41112,name,countable,na,eq,1)-1),- (predicate(_107538,_41144,be,named(f1),_41112)-1)),&(v(- (object(_107538,_41778,codec,countable,na,eq,1)-1),- (predicate(_107538,_41810,be,named(s10),_41778)-1)),&(v(- (object(_107538,_42444,codec,countable,na,eq,1)-1),- (predicate(_107538,_42476,be,named(s11),_42444)-1)),&(v(- (object(_107538,_43110,file,countable,na,eq,1)-1),- (predicate(_107538,_43142,be,named(s10),_43110)-1)),&(v(- (object(_107538,_43776,file,countable,na,eq,1)-1),-(predicate(_107538,_43808,be,named(s11),_43776)-1)),&(v(- (object(_107538,_44442,name,countable,na,eq,1)-1),- (predicate(_107538,_44474,be,named(s10),_44442)-1)),v(- (object(_107538,_45108,name,countable,na,eq,1)-1),- (predicate(_107538,_45140,be,named(s11),_45108)-1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))).

ut_fol_sanity_checks_are_passing :-
     test(_,P,_,X), u(P,X,O), phrase(pddl(O,X), _), !.

ut_tag_sanity_checks_are_passing :-
    tag(A, i(A), []),
    tag(named(b), n(b), [n(b)]),
    must_throw(tag(named(C),_,_), type_error(ground|var,named(C))),
    must_throw(tag(bad(t),_,_), domain_error(named|string,bad(t))).

all_tests_are_passing :-
    %ut_fol_sanity_checks_are_passing,
     ut_tag_sanity_checks_are_passing.

must_throw(Goal, Exception) :-
    X = _,
    catch(Goal, Exception, (X=1)),
    ground(X).

must_not_exist(Predicate) :-
    must_throw(Predicate, error(existence_error(procedure,_),_)) ->
        true;
        throw(error(should_not_exist(Predicate))).
