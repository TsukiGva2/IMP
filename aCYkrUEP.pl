% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% 2025 -- Rodrigo Monteiro Junior
% IMP.pl

:- use_module(library(yall)).

operator(lparen) -->
	[C],
    { char_code('(', C)
    }.
operator(rparen) -->
	[C],
    { char_code(')', C)
    }.

c_operator(semicolon) -->
    [C],
    { char_code(';', C)
    }.
c_operator(assign) -->
    [Colon,Eq],
    { char_code(':', Colon),
      char_code('=', Eq)
    }.

a_operator(plus) -->
    [C],
    { char_code('+', C)
    }.
a_operator(minus) -->
    [C],
    { char_code('-', C)
    }.
a_operator(mul) -->
    [C],
    { char_code('*', C)
    }.

b_unary_operator(not) -->
    [C],
    { char_code('~', C)
    }.
b_operator(and) -->
    [C],
    { char_code('&', C)
    }.
b_operator(or) -->
    [C],
    { char_code('|', C)
    }.
b_operator(eq) -->
    [C],
    { char_code('=', C)
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

% Parse an integer like: "1234"

integer(I) -->
        digit(D0),
        digits(D),
        { number_codes(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D), !,
        digits(T).

digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.
% -----------------------------------

% Tokenizer

lex([]) --> [].
lex([Token|Tokens]) -->
    aexpr(Token),
    lex(Tokens).

% com------------------------------
com(com(C)) -->
    identifier(I),
    c_operator(assign), !,
    aexpr(A).

% ---------------------------------

% boolean--------------------------
bexpr(bexpr(Operator, B)) -->
    b_unary_operator(Operator), !,
    bterm(B).
bexpr(bexpr(B0, Operator, B1)) -->
    bterm(B0),
    b_operator(Operator), !,
    bterm(B1).
bexpr(bexpr(B)) -->
    bterm(B).

bterm(true) --> identifier(true).
bterm(false) --> identifier(false).
bterm(paren(Enclosed)) -->
    operator(lparen),
    bexpr(Enclosed),
    operator(rparen).
%----------------------------------

% arithmetic-----------------------
aexpr(aexpr(A0, Operator, A1)) -->
    aterm(A0),
    a_operator(Operator), !,
    aterm(A1).
aexpr(aexpr(A)) -->
    aterm(A).

aterm(Term) --> term(Term), !.
aterm(paren(Enclosed)) -->
    operator(lparen),
    aexpr(Enclosed),
    operator(rparen).
%----------------------------------

% Basic types
term(int(Integer))  --> integer(Integer), !.
term(loc(Location)) --> identifier(Location).

% -----------------------------------

evaluate :-
    read_line_to_codes(user_input, Line),
    phrase(lex(Tokens), Line),
    write(Tokens).

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
