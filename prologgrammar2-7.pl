% Define a grammar using proper DCG notation that covers:
% 1) Adverbs
% 2) Adjectives
% 3) Relative clauses
% 4) Verbs of arity 1, 2, 3, 4 
% 5) Declarative sentences
% 6) Questions (yes/no “is … ?”, adverbial “when … ?”, role-queried [agent of verb is the answer])
% 7) Sentence-embedding verbs (say/believe/think etc.)
% 8) Question-embedding verbs

% IMPLEMENTATION BY LUKE ALEXANDER BYRNE STUDENT #: 16320031

% Initial Prolog Theory of Grammar (taken and modified from [prologgrammar1.pl].)
:- consult([utilities1,testsuite1a]).
:- unknown(_,warning).

%%% Vocabulary items used in testsuite1a.pl %%%
% DETERMINERS
lex(det,the,_).
lex(det,an,sg).
lex(det,a,sg).

% NOUNS
lex(n,man,sg).
lex(n,men,pl).
lex(n,couch,sg).
lex(n,couches,pl).
lex(n,apple,sg).
lex(n,apples,pl).
lex(n,tree,sg).
lex(n,trees,pl).
lex(n,book,sg).
lex(n,books,pl).

% PERSONAL PRONOUNS
lex(pro,i,1,sg,nom).
lex(pro,me,1,sg,obj).
lex(pro,me,1,sg,dat).
lex(pro,you,2,sg,nom).
lex(pro,you,2,sg,obj).
lex(pro,you,2,sg,dat).
lex(pro,she,3,sg,nom).
lex(pro,her,3,sg,obj).
lex(pro,her,3,sg,dat).

% RELATIVE PRONOUNS (QUESTION 3)
lex(relpro,who). % wh-relatives (subj + non-subj)
lex(relpro,which).
lex(relpro,that). 

% Complementiser (Question 3)
lex(c,that).

% PREPOSITIONS
lex(p,on).
lex(p,to).
lex(p,under).
lex(p,for).

% ADVERBS (QUESTION 1)
% Adverbs
lex(adv,quietly).
lex(adv,quickly).
lex(adv,gently).

% Degree Adverbs
lex(deg,very).
lex(deg,quite).
lex(deg,too).

% QUESTION WORDS (QUESTION 6)
lex(q,who).
lex(q,which).
lex(q,where).
lex(q,when).
lex(q,why).
lex(q,what).

% ADJECTIVES (QUESTION 2)
lex(a,beautiful).
lex(a,comfortable).
lex(a,delicious).
lex(a,important).
lex(a,interesting).
lex(a,happy).

% COPULAR VERBS
lex(vc,am,1,sg).
lex(vc,are,2,sg).
lex(vc,is,3,sg).
lex(vc,are,1,pl).
lex(vc,are,2,pl).
lex(vc,are,3,pl).

% INTRANSITIVE VERBS
lex(vi,sleep,1,sg).
lex(vi,sleep,2,sg).
lex(vi,sleeps,3,sg).
lex(vi,sleep,1,pl).
lex(vi,sleep,2,pl).
lex(vi,sleep,3,pl).
lex(vi,sleep,_,inf).

% TRANSITIVE VERBS
lex(vt,like,1,sg).
lex(vt,like,2,sg).
lex(vt,likes,3,sg).
lex(vt,like,1,pl).
lex(vt,like,2,pl).
lex(vt,like,3,pl).
lex(vt,like,_,inf).

lex(vt,eat,1,sg).
lex(vt,eat,2,sg).
lex(vt,eats,3,sg).
lex(vt,eat,1,pl).
lex(vt,eat,2,pl).
lex(vt,eat,3,pl).
lex(vt,eat,_,inf).

% DITRANSITIVE VERBS
lex(vd,give,1,sg).
lex(vd,give,2,sg).
lex(vd,gives,3,sg).
lex(vd,give,1,pl).
lex(vd,give,2,pl).
lex(vd,give,3,pl).
lex(vd,give,_,inf).

% TRITRANSITIVE VERBS (DITRANSIVE WITH ADJUNCT)
lex(vd,trade,1,sg).
lex(vd,trade,2,sg).
lex(vd,trades,3,sg).
lex(vd,trade,1,pl).
lex(vd,trade,2,pl).
lex(vd,trade,3,pl).
lex(vd,trade,_,inf).

% QUESTION-EMBEDDING TRANSITIVE VERBS
lex(vq,know,1,sg).
lex(vq,know,2,sg).
lex(vq,knows,3,sg).
lex(vq,know,1,pl).
lex(vq,know,2,pl).
lex(vq,know,3,pl).

% AUXILLARY VERBS
lex(va,am,1,sg).
lex(va,are,2,sg).
lex(va,is,3,sg).
lex(va,are,1,pl).
lex(va,are,2,pl).
lex(va,are,3,pl).

lex(va,do,1,sg).
lex(va,do,2,sg).
lex(va,does,3,sg).
lex(va,do,1,pl).
lex(va,do,2,pl).
lex(va,do,3,pl).

lex(va,can,_,_).
lex(va,will,_,_).


%%% PHRASE STRUCTURE RULES %%%
% Noun Phrases
np([np(Num, Case), [DET, N]], 3, Num, Case) --> det(DET, Num), n(N, Num).
np([np(Num, Case), [PN]], Per, Num, Case) --> pro(PN, Per, Num, Case).

% Noun Phrase with Attributive AP
np([np(Num, Case), [DET, A, N]], 3, Num, Case) --> det(DET, Num), ap(A), n(N, Num).

% Noun Phrase with Relative Clause
np2([np(Num,Case), [NP, CP]], Per, Num, Case) --> np(NP,Per,Num,Case), cp(CP).


% Verb Phrases
% Higher-order VPs with adjuncts
vp2([vp(Num), [VP, A]], Per, Num) --> vp(VP, Per, Num), adjunct(A). % gives ... under the tree
vp2([vp(Num), [A, VP]], Per, Num) --> adjunct(A), vp(VP, Per, Num). % gently gives ... 
vp2([vp(Num), [A1, VP, A2]], Per, Num) --> adjunct(A1), vp(VP, Per, Num), adjunct(A2). % gentle gives ... under the tree

% Copular VPs
scomp([scomp(Num), [A]]) --> ap(A). % Predicative AP in SCOMP
scomp([scomp(Num), [A, CP]]) --> ap(A), cp(CP). % with relative clause as argument
vp([vp(Num), [V, SComp]], Per, Num) --> vc(V, Per, Num), scomp(SComp). % with scomp

% Intransitive VPs
vp([vp(Num), [V]], Per, Num) --> vi(V, Per, Num).

% Transitive VPs (Question 4 & Question 8)
vp([vp(Num), [V, N]], Per, Num) --> vt(V, Per, Num), np(N,_,_,obj).
vp([vp(Num), [V, CP]], Per, Num) --> vt(V, Per, Num), cp(CP). % with relative clause
vp([vp(Num), [V, QP]], Per, Num) --> vq(V, Per, Num), s(S). % question-embedded transitive verbs

% Ditransitive VPs
% SUBJ V(Ditrans) OBJ-dir OBJ-indir
vp([vp(Num), [V, N, P]], Per, Num) --> vd(V, Per, Num), np(N,_,_,obj), pp(P, dat). % pp as arg
vp([vp(Num), [V, N, A, P]], Per, Num) --> vd(V, Per, Num), np(N,_,_,obj), advp(A), pp(P, dat).
	% not best way to represent this and not syntactically consistent, but easiest way at the moment
	% to represent an adverb phrase (not adjunct as its modifying the verb) before the indirect object, 
	% but after the direct object, keeping in mind that an adverbial cannot 
	% intersect a verb and its direct object
% SUB V(Ditrans) OBJ-indir OBJ-dir
vp([vp(Num), [V, N1, N2]], Per, Num) --> vd(V, Per, Num), np(N1,_,_,dat), np(N2,_,_,obj).
vp([vp(Num), [V, CP]], Per, Num) --> vd(V, Per, Num), cp(CP). % with relative clause


% Prepositional Phrase
pp([pp, [P, NP]], dat) --> p(P), np(NP,_,_,dat). % to me (ditrans)
pp([pp, [P, NP]], Case) --> p(P), np(NP,_,_,obj), {Case \== dat}. % under the tree


% Adjunct and Adjunct Adverbial Phrase
adjunct([adjunct, PP]) --> pp(PP, Case), {Case \== dat}. % on the couch
adjunct([adjunct, AdvP]) --> advp(AdvP). % quietly
adjunct([adjunct, [PP, A]]) --> pp(PP, Case), adjunct(A), {Case \== dat}. % recursive adjuncts
adjunct([adjunct, [AdvP, A]]) --> advp(AdvP), adjunct(A). % recursive adjuncts


% Adverb Phrase
advp([advp, Adv]) --> adv(Adv).
advp([advp, [Deg, Adv]]) --> deg(Deg), adv(Adv).


% Adjective Phrase
ap([ap, A]) --> a(A).
ap([ap, [Deg, A]]) --> deg(Deg), a(A).
ap([ap, [AdvP, A]]) --> advp(AdvP), a(A).


% Complementiser Phrases (Relative Clauses)
cp([cp, [C, NP, VP]]) --> c(C), s([s, [NP,VP]]).


% Sentential Phrase
% Interrogative Sentential Phrases (Question 6)
s([s, [AUX, S]]) --> interr(AUX), sbar(S).
interr([interr, AUX]) --> va(AUX, Per, Num).
sbar([s, [NP, VP]]) --> np(NP, Per, Num, nom), vp(VP, Per, inf).
sbar([s, [NP, SComp]]) --> np(NP, Per, Num, nom), scomp(SComp). % copular verb questions

% Interrogative Sentential Phrases with Copular Verbs/Be Auxillary (Question 6)
s([s, [Q, AUXP]]) --> q(Q), interrp(AUXP).
interrp([interrp, [AUX, S]]) --> va(AUX, Per, Num), sbar(S).

% Question-embedding Sentences
s([s, [Q, S]]) --> q(Q), s(S).

% Basic Sentential Phrases
s([s, [NP, VP]]) --> np(NP, Per, Num, nom), vp(VP, Per, Num).
s([s, [NP, VP]]) --> np(NP, Per, Num, nom), vp2(VP, Per, Num).
s([s, [A, NP, VP]]) --> adjunct(A), np(NP, Per, Num, nom), vp(VP, Per, Num). % adjunct-initial
s([s, [A, NP, VP]]) --> adjunct(A), np(NP, Per, Num, nom), vp2(VP, Per, Num). % adjunct-initial


%%% PHRASE STRUCTURE PRETERMINAL RULES %%%
% Determiners
det([det(Num), [W]], Num) --> [W], {lex(det,W,Num)}.

% Nouns
n([n(Num), [W]], Num) --> [W], {lex(n,W,Num)}.

% Pronouns
pro([pro(Per, Num, Case), [W]], Per, Num, Case) --> [W], {lex(pro,W,Per,Num,Case)}.

% Verbs
v([v(Num), [W]], Per, Num) --> [W], {lex(v,W,Per,Num)}.
vc([v(Num), [W]], Per, Num) --> [W], {lex(vc,W,Per,Num)}.
vi([vi(Num), [W]], Per, Num) --> [W], {lex(vi,W,Per,Num)}.
vt([vt(Num), [W]], Per, Num) --> [W], {lex(vt,W,Per,Num)}.
vd([vd(Num), [W]], Per, Num) --> [W], {lex(vd,W,Per,Num)}.
va([va(Num), [W]], Per, Num) --> [W], {lex(va,W,Per,Num)}.
vq([vq(Num), [W]], Per, Num) --> [W], {lex(vq,W,Per,Num)}.

% Prepositions
p([p, [W]]) --> [W], {lex(p,W)}.

% Adverbs
adv([adv, [W]]) --> [W], {lex(adv,W)}.
deg([deg, [W]]) --> [W], {lex(deg,W)}.

% Adjectives
a([a, [W]]) --> [W], {lex(a,W)}.

% Relative Pronouns
relpro([relpro, [W]]) --> [W], {lex(relpro,W)}.

% Complementiser
c([c, [W]]) --> [W], {lex(c,W)}.

% Interrogatives
q([q, [W]]) --> [W], {lex(q,W)}.



%%% TOOLS %%%
% Pretty Printing
drawtree(Tree) :-
	d_tree(Tree, 0).

d_tree([],_).
d_tree([Mother|Daughters], Indent) :-
	nonlist(Mother),!,
	tab(Indent),
	write(Mother),
	calcindent(Indent, NewIndent),
	nl,d_daughters(Daughters, NewIndent).
	
d_tree([Mother|Daughters], Indent) :-
	d_tree(Mother, Indent),
	d_tree(Daughters, Indent).

nonlist(Item) :- functor(Item,X,Y), X \== '.'.

d_daughters([],_).
d_daughters([First|Rest], Indent) :-
	nonvar(First),
	d_tree(First, Indent),
	d_daughters(Rest, Indent).
	
calcindent(N, N1) :-
	N1 is N + 2.