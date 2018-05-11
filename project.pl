chat:-
 repeat,
   readinput(Input),
   process(Input), 
  (Input = [bye| _] ),!.
  

% ===========================================================
% Read input:
% 1. Read char string from keyboard. 
% 2. Convert char string to atom char list.
% 3. Convert char list to lower case.
% 4. Tokenize (based on spaces).
% ===========================================================

readinput(TokenList):-
   read_line_to_codes(user_input,InputString),
   string_to_atom(InputString,CharList),
   string_lower(CharList,LoweredCharList),
   tokenize_atom(LoweredCharList,TokenList).


% ===========================================================
%  Process tokenized input
% 1. Parse morphology and syntax, to obtain semantic representation
% 2. Evaluate input in the model
% If input starts with "bye" terminate.
% ===========================================================

process(Input):-
	parse(Input,SemanticRepresentation),
	modelchecker(SemanticRepresentation,Evaluation),
	respond(Evaluation),!,
	nl,nl.
	
process([bye|_]):-
   write('> bye!').


% ===========================================================
%  Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

parse(Sentence,SemRep):-
        doparse([],Sentence,SemRep).

doparse([X],[],X).

doparse([Y,X|MoreStack],Words,SemRep):-
       rule(LHS,[X,Y]),
       doparse([LHS|MoreStack],Words,SemRep),!.

doparse([X|MoreStack],Words,SemRep):-
       rule(LHS,[X]),
       doparse([LHS|MoreStack],Words,SemRep).

doparse(Stack,[Word|Words],SemRep):-
        lex(X,Word),
        doparse([X|Stack],Words,SemRep).


% ===========================================================
% Grammar
% 1. List of lemmas
% 2. Lexical items
% 3. Phrasal rules
% ===========================================================

% --------------------------------------------------------------------
% Lemmas are uninflected, except for irregular inflection
% lemma(+Lemma,+Category)
% --------------------------------------------------------------------

lemma(bowl,n).
lemma(box,n).
lemma(shelf,n).
lemma(thing,n).
lemma(egg,n).
lemma(freezer,n).
lemma(ham,n).
lemma(container,n).
lemma(sandwich,n).
lemma(meat,n).
lemma(watermelon,n).
lemma(fridge,n).
lemma(milk,n).
lemma(popsicle,n).

lemma(who,whpr).
lemma(what,whpr).
lemma(which,whpr).

lemma(there,pron).
lemma(i,pron).
lemma(me,pron).
lemma(he,pron).
lemma(him,pron).
lemma(she,pron).
lemma(her,pron).
lemma(it,pron).
lemma(we,pron).
lemma(us,dtexists).
lemma(they,pron).
lemma(them,pron).

lemma(tom,pn).
lemma(mia,pn).
lemma(sam,pn).
lemma(sue,pn).


lemma(a,dtexists).
lemma(an,dtexists).
lemma(the,dtexists).
lemma(some,dtexists).
lemma(my,dtexists).
lemma(his,dtexists).
lemma(her,dtexists).
lemma(our,dtexists).
lemma(their,dtexists).
lemma(zero,dtexists).
lemma(one,dtexists).
lemma(two,dtexists).
lemma(three,dtexists).
lemma(four,dtexists).
lemma(five,dtexists).
lemma(six,dtexists).
lemma(seven,dtexists).
lemma(eight,dtexists).
lemma(nine,dtexists).
lemma(ten,dtexists).

lemma(each,dtforall).
lemma(all,dtforall).
lemma(every,dtforall).

lemma(no,dtnotexist).


lemma(red,adj).
lemma(blue,adj).
lemma(yellow,adj).
lemma(green,adj).
lemma(white,adj).
lemma(top,adj).
lemma(middle,adj).
lemma(bottom,adj).
lemma(almond,adj).
lemma(empty,adj).
lemma(ham,adj).

lemma(not,adv).


lemma(exist,iv).
lemma(rot,iv).
lemma(ripe,iv).
lemma(ripen,iv).

lemma(eat,tv).
lemma(ate,tv).
lemma(contain,tv).
lemma(belong,tv).

lemma(put,dtv).
lemma(place,dtv).
lemma(add,dtv).
lemma(remove,dtv).


lemma(in,p).
lemma(under,p).
lemma(top,p).
lemma(inside,p).
lemma(with,p).
lemma(of,p).
lemma(on,p).   
lemma(to,p).
lemma(at,p).


lemma(is,aux).
lemma(was,aux).
lemma(am,aux).
lemma(were,aux).
lemma(are,aux).
lemma(do,aux).
lemma(did,aux).
lemma(does,aux).
lemma(have,aux).
lemma(has,aux).
lemma(had,aux).
lemma(can,aux).
lemma(could,aux).
lemma(may,aux).
lemma(will,aux).
lemma(should,aux).
lemma(would,aux).


lemma(that,rel).
lemma(which,rel).
lemma(who,rel).


is_a(ham,meat).
is_a(meat,food).
is_a(watermelon,food).
is_a(egg,food).
is_a(sandwich,food).
is_a(popsicle,food).
is_a(milk,beverage).
is_a(water,beverage).
is_a(X,thing):-	lemma(X,n).
is_a(X,thing):-	is_a(_,X).

% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------


lex(n(X^P),Word):-
  	uninflect0(Word,Lemma),
	lemma(Lemma,n),
	P=.. [Lemma,X].
lex(whpr(X^P),Lemma):-
	lemma(Lemma,whpr),
	P=.. [Lemma,X].
lex(pron(X^P),Lemma):-
	lemma(Lemma,pron),
	P=.. [Lemma,X].
lex(pn((Name^X)^X),Name):-
	lemma(Name,pn).


lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Word):-
	lemma(Word,dtforall).
lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Word):-
	lemma(Word,dtexists).
lex(dt((X^P)^(X^Q)^notexist(X,and(P,Q))),Word):-
	lemma(Word,dtnotexist).


lex(adj((X^P)^X^and(P,K)),Lemma):-
	lemma(Lemma,adj),
	K=.. [Lemma,X].
lex(adv((X^P)^X^and(P,K)),Lemma):-
	lemma(Lemma,adv),
	K=.. [Lemma,X].


lex(p((Y^K)^Q^(X^P)^and(P,Q)),Lemma):-
	lemma(Lemma,p),
	K=.. [Lemma,X,Y].


lex(iv((X^P),[]),Word):-
    uninflect0(Word,Lemma),
	lemma(Lemma,iv),
	P=.. [Lemma,X].
lex(tv((K^W^P),[]),Word):-
  	uninflect0(Word,Lemma),
   	lemma(Lemma,tv),
	P=.. [Lemma,K,W].
lex(dtv((K^W^J^P),[]),Word):-
    	uninflect0(Word,Lemma),
	lemma(Lemma,dtv),
	P=.. [Lemma,K,W,J].

lex(aux,Lemma):-
    lemma(Lemma,aux).
      
lex(rel,Lemma):-
    lemma(Lemma,rel).

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------

uninflect0(X,Y):- lemma(X,_), Y=X.
uninflect0(X,Y):- atom_concat(A,B,X), lemma(A,_), uninflect1(B),Y=A.
uninflect1(‘’).
uninflect1(X):- noun_inflection(X),!.
uninflect1(X):- verb_inflection(X),!.

noun_inflection(s).
noun_inflection(es).
verb_inflection(s).
verb_inflection(ing).
verb_inflection(ed).

tag(X,Y,Z):- uninflect0(X,K),Y=K, lemma(K,Z).


% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

rule(np(Y),[dt(X^Y),n(X)]).
rule(np(X),[pn(X)]).
rule(np(X),[pron(X)]).
rule(np(X),[n(X)]).

rule(s(Y),[np(X^Y),vp(X)]).

rule(n(Y),[adv(X^Y),n(X)]).
rule(n(Y),[adj(X^Y),n(X)]).

rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).
rule(vp(X),[aux,pp(X)]).

rule(vp(X),[iv(X)]).
rule(vp(X^W),[tv(X^Y),np(Y^W)]).
rule(vp(X^W),[dtv(X^Y^K),np(Y^W),np(K^W)]).
rule(vp(X^W),[aux,np(Y^W)]).

rule(s(Y,WH),[np(X^Y),vp(X,WH)]).
rule(vp(X,WH),[iv(X,WH)]).
rule(vp(X^K,[]),[tv(X^Y,[]),np(Y^K)]).
rule(vp(X^K,[]),[dtv(X^Y^J,[]),np(Y^K),np(J,K)]).

rule(s(X,[WH]),[vp(X,[WH])]).
rule(vp(K,[WH]),[tv(Y,[WH]),np(Y^K)]).
rule(vp(K,[WH]),[dtv(Y^J,[WH]),np(Y^K),np(J^K)]).

rule(Y,[whpr(X^Y),vp(X,[])]).
rule(ynq(Y),[aux,np(X^Y),vp(X,[])]).
rule(Z,[whpr((X^Y)^Z),inv_s(Y,[X])]).
rule(inv_s(Y,[WH]),[aux,np(X^Y),vp(X,[WH])]).

rule(n(X^and(Y,Z)),[n(X^Y),rc(X^Z,[])]).
rule(n(X^and(Y,Z)),[n(X^Y),rc(Z,[X])]).


% ===========================================================
%  Modelchecker:
%  1. If input is a declarative, check if true
%  2. If input is a yes-no question, check if true
%  3. If input is a content question, find answer
% ===========================================================

% model(...,...)

% ===========================================================
%  Respond
%  For each input type, react appropriately.
% ===========================================================

% Declarative true in the model
respond(Evaluation) :- 
		Evaluation = [true_in_the_model], 
		write('That is correct'),!.

% Declarative false in the model
respond(Evaluation) :- 
		Evaluation = [not_true_in_the_model],  
		write('That is not correct'),!.

% Yes-No interrogative true in the model
respond(Evaluation) :- 
		Evaluation = [yes_to_question],			
		write('yes').

% Yes-No interrogative false in the model		
respond(Evaluation) :- 
		Evaluation = [no_to_question], 			
		write('no').

% wh-interrogative true in the model
% ...							

% wh-interrogative false in the model
