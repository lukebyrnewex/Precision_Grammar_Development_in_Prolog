% cmv testsuite1
% Last Modified: Thu Jan 30 12:50:02 2020 (vogel) 
% starting point at end of the lecture
% A DCG with Case, Person, Speaker and Number Agreement
% (use utilities1.pl)

% X is a variable to be instantiated with the PARSE
% representation.

% IMPLEMENTATION BY LUKE ALEXANDER BYRNE, STUDENT #: 16320031


% BASIC TESTS
test(1,X) :- s(X,[the,man,sleeps,on,the,couch],[]). % pass
test(2,X) :- s(X,[the,men,sleep,on,the,couch],[]). % pass
test(3,X) :- s(X,[the,man,sleep,on,the,couches],[]). % fail
test(4,X) :- s(X,[the,men,sleeps,on,the,couches],[]). % fail
test(5,X) :- s(X,[the,man,sleeps],[]). % pass
test(6,X) :- s(X,[she,gives,the,couch,to,i],[]). % fail
test(7,X) :- s(X,[she,gives,the,couch,to,me],[]). % pass
test(8,X) :- s(X,[her,gives,the,couch,to,me],[]). % fail
test(9,X) :- s(X,[i,give,the,couch,to,her],[]). % pass
test(10,X) :- s(X,[i,gives,the,couch,to,her],[]). % fail
test(11,X) :- s(X,[i,sleeps],[]). % fail
test(12,X) :- s(X,[i,sleep],[]). % pass
test(13,X) :- s(X,[she,sleeps],[]). % pass 
test(14,X) :- s(X,[she,sleep],[]). % fail
test(15,X) :- s(X,[her,sleeps],[]). % fail

% ADVERB TESTS (Question 1)
% Intransitive
test(adv1a,X) :- s(X, [quietly,the,man,sleeps],[]). % pass
test(adv1b,X) :- s(X, [the,quietly,man,sleeps],[]). % fail
test(adv1c,X) :- s(X, [the,man,quietly,sleeps],[]). % pass
test(adv1d,X) :- s(X, [the,man,sleeps,quietly],[]). % pass
test(adv2a,X) :- s(X, [the,man,sleeps,quietly,on,the,couch],[]). % pass
test(adv2b,X) :- s(X, [the,man,sleeps,on,the,couch,quietly],[]). % pass

% Transitive
test(adv3a,X) :- s(X, [quietly,she,eats,the,apples],[]). % pass
test(adv3b,X) :- s(X, [she,quietly,eats,the,apples],[]). % pass
test(adv3c,X) :- s(X, [she,eats,quietly,the,apples],[]). % fail
test(adv3d,X) :- s(X, [she,eats,the,quietly,apples],[]). % fail
test(adv3e,X) :- s(X, [she,eats,the,apples,quietly],[]). % pass
test(adv4a,X) :- s(X, [she,eats,the,apples,quietly,under,the,tree],[]). % pass
test(adv4b,X) :- s(X, [she,eats,the,apples,under,the,tree,quietly],[]). % pass

% Ditransitive (can't come between vd and its direct object)
test(adv5a,X) :- s(X, [gently,she,gives,the,book,to,me],[]). % pass
test(adv5b,X) :- s(X, [she,gently,gives,the,book,to,me],[]). % pass
test(adv5c,X) :- s(X, [she,gives,gently,the,book,to,me],[]). % fail
test(adv5d,X) :- s(X, [she,gives,the,book,gently,to,me],[]). % pass
test(adv5e,X) :- s(X, [she,gives,the,book,to,me,gently],[]). % pass
test(adv6a,X) :- s(X, [gently,she,gives,me,the,book],[]). % pass
test(adv6b,X) :- s(X, [she,gently,gives,me,the,book],[]). % pass
test(adv6c,X) :- s(X, [she,gives,gently,me,the,book],[]). % fail
test(adv6d,X) :- s(X, [she,gives,me,gently,the,book],[]). % fail
test(adv6e,X) :- s(X, [she,gives,me,the,book,gently],[]). % pass
test(adv7a,X) :- s(X, [gently,she,gives,me,the,book,under,the,tree],[]). % pass
test(adv7b,X) :- s(X, [she,gently,gives,me,the,book,under,the,tree],[]). % pass
test(adv7c,X) :- s(X, [she,gives,me,the,book,gently,under,the,tree],[]). % pass
test(adv7d,X) :- s(X, [she,gives,me,the,book,under,the,tree,gently],[]). % pass

% Degree Adverbs
test(adv8a,X) :- s(X, [she,gives,me,the,book,very,quietly],[]).  % pass
test(adv8b,X) :- s(X, [she,eats,the,apples,too,quickly],[]). % pass
test(adv8c,X) :- s(X, [she,eats,the,apples,quickly,quite],[]). % fail

% ADJECTIVES (Question 2)
% Copular Verbs with Predicative Adjectives as S-COMP
test(adj1a,X) :- s(X, [you,are,beautiful],[]). % pass
test(adj1b,X) :- s(X, [the,men,are,beautiful],[]). % pass
test(adj1c,X) :- s(X, [the,apples,are,delicious],[]). % pass
test(adj1d,X) :- s(X, [the,couches,are,very,comfortable],[]). % pass
test(adj1e,X) :- s(X, [she,eats,the,apples,comfortable],[]). % fail

% Attributive APs with Nouns
test(adj2a,X) :- s(X, [the,beautiful,men,sleep,under,the,tree],[]). % pass
test(adj2b,X) :- s(X, [the,couches,are,too,comfortable],[]). % pass

% RELATIVE CLAUSES (Question 3)
test(rel1a,X) :- s(X, [i,like,that,you,like,her],[]). % pass 
test(rel1b,X) :- s(X, [i,am,very,happy,that,you,like,me],[]). % pass

% QUESTIONS (Question 6)
% 6a) Yes/No Questions (Is ... ?)
test(qst1a,X) :- s(X, [is,the,couch,comfortable],[]). % pass, vc
test(qst1b,X) :- s(X, [is,the,book,interesting],[]). % pass, vc
test(qst1c,X) :- s(X, [are,the,apples,delicious],[]). % pass, vc

test(qst2a,X) :- s(X, [will,you,eat,the,apples],[]). % pass, vt
test(qst2b,X) :- s(X, [can,you,sleep],[]). % pass, vi
test(qst2c,X) :- s(X, [will,you,give,the,man,the,apples],[]). % pass, vd
test(qst2d,X) :- s(X, [can,she,give,you,the,couch],[]). % pass, test for inf form of "she give"
test(qst2e,X) :- s(X, [can,she,gives,you,the,couch],[]). % fail, test for inf form of "she give"

% 6b) When/where etc. Questions (When... ?)
test(qst3a,X) :- s(X, [where,does,she,eat,the,apples],[]). % pass/trans
test(qst3b,X) :- s(X, [when,do,the,men,give,the,books,to,her],[]). % pass/ditrans

% TRANSITIVE VERB TESTS (Question 4)
test(vt1,X) :- s(X, [the,man,likes,the,couch],[]). % pass
test(vt2,X) :- s(X, [the,man,likes],[]). % fail
test(vt3,X) :- s(X, [the,man,likes,me,the,couch],[]). % fail

% TRITRANSITIVE VERB TESTS (Question 4)
test(vd1,X) :- s(X, [the,man,trades,her,an,apple,for,a,book],[]).

% QUESTION-EMBEDDING VERBS (Question 8)
test(qev1,X) :- s(X, [i,know,where,she,eats,the,apples],[]).
test(qev2,X) :- s(X, [i,know,when,she,sleeps],[]).


testem :-
	pass([1,2,5,7,9,12,13,
		adv1a,adv1c,adv1d,
		adv2a,adv2b,
		adv3a,adv3b,adv3e,
		adv4a,adv4b,
		adv5a,adv5b,adv5d,adv5e,
		adv6a,adv6b,adv6e,
		adv7a,adv7b,adv7c,adv7d,
		adv8a,adv8b,
		adj1a,adj1b,adj1c,adj1d,
		adj2a,adj2b,
		rel1a,rel1b,
		qst1a,qst1b,qst1c,
		qst2a,qst2b,qst2c,qst2d,
		qst3a,qst3b,
		vt1,
		vd1,
		qev1,qev2]),
	fail([3,4,6,8,10,11,14,15,
		adv1b,
		adv3c,adv3d,
		adv5c,
		adv6c,adv6d,
		adv8c,
		adj1e,
		qst2e,
		vt2,vt3]).