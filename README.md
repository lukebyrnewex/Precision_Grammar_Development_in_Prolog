# Precision_Grammar_Development_in_Prolog
Program which verifies, using Definite Clause Grammar (DCG) notation and established grammar development in Prolog, whether a phrase is grammatical or not.

## 1) Adverbs
	-- implemented in all positions whether adjunct or otherwise for all verbs
## 2) Adjectives
	-- implemented in attributive adjectival position (the beautiful man) and in adjective phrase structures,
		including degree adverb + adjective (very beautiful) and as an SCOMP to copular verbs (she is beautiful)
## 3) Relative clauses
	-- implemented thus far as an object to a transitive verb (I like that you like her)
## 4) Verbs of arity 1, 2, 3, 4 
	-- arity 1: copular verbs with scomp implemented (be)
	-- arity 2: transitive verbs implemented (like/eat)
	-- arity 3: ditransitive verbs implemented (give) in both prepositional form (I give the book to her) and dative shift
		form (I give her the book), with the introduction of a dative case
	-- arity 4: ditransitive verbs with prepositional argument (tritransitive)
## 5) Declarative sentences
	-- implemented in all forms as "normal" sentences
## 6) Questions (yes/no “is … ?”, adverbial “when … ?”, role-queried [agent of verb is the answer])
	-- YES/NO: implemented with interrogative phrase + sbar
	-- When/where: implemented for standard SVO structure (when will you eat the apples/can she give you the couch),
		including the exclusion of sentences in the wrong form (i.e. *can she gives you the couch), with implementation
		of infintival verb forms under NUM (inf/bse could have been implemented as separate parameter but for brevity
		I used it alongside sg/pl/inf.
			-- implementation of auxiliary verbs and sbar constructions achieved in order to allow for sentences not beginning with a noun phrase in nominative form
## 7) Sentence-embedding verbs
	-- implemented in general
## 8) Question-embedding verbs
	-- implemented with the verb "to know" as an example

