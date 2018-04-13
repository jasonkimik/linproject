
% ===========================================================
% Main loop:
% 1. Repeat "input-response" cycle until input starts with "bye"
%    Each "input-response" cycle consists of:
% 		1.1 Reading an input string and convert it to a tokenized list
% 		1.2 Processing tokenized list
% ===========================================================

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

%parse(Input, SemanticRepresentation):-
% ...


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


lemma(tom,pn).
lemma(mia,pn).
lemma(sam,pn).
lemma(sue,pn).

lemma(a,dtexists).
lemma(an,dtexists).
lemma(some,dtexists).

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

lemma(exist,iv).

lemma(eat,tv).
lemma(contain,tv).
lemma(belong,tv).
lemma(have,tv).
lemma(has,tv).
lemma(had,tv).
lemma(drink,tv).
lemma(drank,tv).
lemma(drunk,tv).

lemma(put,dtv).

lemma(in,p).
lemma(under,p).

lemma(on,vacp).   
lemma(to,vacp).

lemma(for,con).
lemma(so,con).
lemma(yet,con).
lemma(but,con).
lemma(nor,con).
lemma(or,con).
lemma(and,con).

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

 
% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------


lex(n(X^P),Lemma):-
	lemma(Lemma,n),
	P=.. [Lemma,X].

lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Word):-
		lemma(Word,dtforall).
				
lex(n(X^P),Lemma):-
	lemma(Lemma,n),
	P=.. [Lemma,X].
lex(pn((Name^X)^X),Name):-
	lemma(Name,pn).

lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Word):-
		lemma(Word,dtforall).
lex(dt((X^P)^(X^Q)^exists(X,imp(P,Q))),Word):-
		lemma(Word,dtexists).
lex(dt((X^P)^(X^Q)^notexist(X,imp(P,Q))),Word):-
	lemma(Word,dtnotexist).

lex(adj((X^P)^X^and(P,K):-
	lemma(Lemma,adj),
	K=.. [Lemma,X].

lex(p((Y^K)^Q^(X^P)^and(P,Q)),Lemma):-
	lemma(Lemma,p),
	K=.. [Lemma,X,Y].

lex(iv(X^P),Lemma):-
	lemma(Lemma,iv),
	P=.. [Lemma,X].



% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------

% ...

% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

rule(np(Y),[dt(X^Y),n(X)]).
rule(np(X),[pn(X)]).

% ...


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
% ...							

