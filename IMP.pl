% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% 2025 -- Rodrigo Monteiro Junior
% IMP.pl

:- use_module(library(dcg/basics)).

paren(open) -->
	[C],
    { code_type(C, paren(_))
    }.
paren(close) -->
	[C],
    { code_type(_, paren(C))
    }.

% Parse an identifier like: "Linger"

identifier(I) -->
    first_symbol(S0),
    symbols(S),
    { atom_codes(I, [S0|S])
    }.

symbols([S|T]) -->
    symbol(S), !,
    symbols(T).

symbols([]) -->
    [].

symbol(S) -->
    [S],
    { code_type(S, csym)
    }.

first_symbol(S) -->
    [S],
    { code_type(S, csymf)
    }.
% -----------------------------------

% com------------------------------

else(Cond, Clause, if(Cond, Clause, Else)) -->
    [0'e], "lse", !, blanks,
    computation(Else).
else(Cond, Clause, if(Cond, Clause)) -->
    [].

parse_if(IF) -->
    blanks,
    bexpr(B),          blanks,
    "then",            blanks,
    computation(Then), blanks,
    else(B, Then, IF).

parse_while(while(B, Clause)) -->
    blanks,
    bexpr(B),         blanks,
    "do",             blanks,
    computation(Clause).

com(I=A) -->
    identifier(I), blanks,
    ":=", !,       blanks,
    aexpr(A).

com(WHILE) -->
    [0'w], "hile", !,
    parse_while(WHILE).

com(IF) -->
    [0'i], [0'f], !,
    parse_if(IF).

com(skip) -->
    [0's], "kip", !.

cterm_next(C0, C0;C1) -->
    [0';], !,
    computation(C1).
cterm_next(C0, C0) -->
    [].

cterm(C) -->
    paren(open), !,
    cterm(C),
    paren(close).
cterm(Term) -->
    com(C),
    cterm_next(C, Term).

computation(C) -->
    cterm(C).

% ---------------------------------

% boolean--------------------------
cop(ge)  --> [0'>], [0'=].
cop(le)  --> [0'<], [0'=].
cop(eq)  --> [0'=].
cop(dif) --> [0'!].

cmp(Cmp) -->
    aterm(A),
    cmp_r(A, Cmp).

cmp_r(A0, Cmp) -->
    cop(Operator),
    aterm(A1),
    { Cmp =.. [Operator, A0, A1]
    }.

bop(and) --> [0'&].
bop(or)  --> [0'+].
bop(if)  --> [0'-], [0'>].
bop(iff) --> [0'<], [0'-], [0'>].

bexpr_r(B0, Expr) -->
    bop(Operator), !,
    bterm(B1),
    bexpr_r(SubExpr, Expr),
    { SubExpr =.. [Operator, B0, B1]
    }.
bexpr_r(B, B) -->
    [].

bexpr(Expr) -->
    bterm(B), !,
    bexpr_r(B, Expr).

bterm(true) --> [0':], [0't], !.
bterm(false) --> [0':], [0'f], !.
bterm(not(Term)) -->
    [0'~], !,
    bterm(Term).
bterm(Enclosed) -->
    paren(open), !,
    bexpr(Enclosed),
    paren(close).

bterm(Cmp) -->
    cmp(Cmp).
%----------------------------------

% arithmetic-----------------------
aop(add) --> [0'+].
aop(sub) --> [0'-].
aop(mul) --> [0'*].
aop(exp) --> [0'^].
aop(div) --> [0'/].

aexpr(Expr) -->
    aterm(A),
    aexpr_r(A, Expr).

aexpr_r(A0, Expr) -->
    aop(Operator), !,
    aterm(A1),
    aexpr_r(SubExpr, Expr),
    { SubExpr =.. [Operator, A0, A1]
    }.
aexpr_r(A, A) -->
    [].

aterm(Term) --> term(Term), !.
aterm(Enclosed) -->
    paren(open),
    aexpr(Enclosed),
    paren(close).
%----------------------------------

% Basic types
term(int(Integer))  --> integer(Integer), !.
term(loc(Location)) --> identifier(Location).

% -----------------------------------

/** <examples>

?- string_codes("<your code here>", Expr), phrase(aexpr(A), Expr).

?- phrase(term(X), [49]). % 1
?- phrase(term(X), [97]). % a

?- phrase(aterm(T), [49]).             % 1
?- phrase(aterm(T), [97,98,99]).       % abc
?- phrase(aterm(T), [40,49,43,49,41]). % (1+1)

?- phrase(aexpr(A), [49,43,49]). % 1 + 1

?- string_codes("1+((4*(2-3))+(5+4))", Expr), phrase(aexpr(A), Expr).

*/
