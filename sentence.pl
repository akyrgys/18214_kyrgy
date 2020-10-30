sentence(S) :- nounPhrase(NP), verbPhrase(VP), append(NP, VP, S).
noun