% 2025 - Rodrigo Monteiro Junior
% IMP.pl

:- use_module(library(clpfd)).

:- det(lex/3).

operator(43,   plus).  % +
operator(45,   minus). % -
operator(172,  not).   % ¬
operator(215,  mul).   % ×
operator(8743, and).   % ∧
operator(8801, eq).    % ≡
operator(8804, le).    % ≤
operator(8874, or).    % ∨
operator(59, semicolon).

lparen(40). % (
rparen(41). % )

space(32).

digit(N)       :- char_type(N, digit).
identifierf(X) :- char_type(X, csymf).
identifier(X)  :- char_type(X, csym).

put(X, C, A) :- append(X, [C], A).

% -------------------------------------
% AEVAL (Σ(ACC, LOC : dict), TOK, OUT)
% ------------------------------------

aV([ACC,   _], [        ], ACC).
aV([nil, LOC], [n(N)|TOK], OUT) :-               aV([N, LOC], TOK, OUT).
aV([nil, LOC], [l(X)|TOK], OUT) :- N=LOC.get(X), aV([N, LOC], TOK, OUT).
aV([ACC, LOC], [o(O)|TOK], OUT) :- aVS().

% base
lex(T, [],[T])               :- !.
lex(nil,  [C|Cs], Ts)        :-   lparen(C), !, lex(p([]), Cs, Ts).
lex(p(E), [C|Cs], Ts)        :-              !, lex(p([C | E]), Cs, Ts).
lex(p(E), [C|Cs], [p(P)|Ts]) :-   rparen(C), !, lex(nil, E, P), lex(nil, Cs, Ts).
lex(nil,  [C|Cs], Ts)        :-    space(C), !, lex(nil, Cs, Ts).
lex(nil,  [C|Cs], Ts)        :-    digit(C), !, lex(n([C]), Cs, Ts).
lex(n(X), [C|Cs], Ts)        :-    digit(C), !, put(X,C,A), lex(n(A), Cs, Ts).
lex(n(X), [C|Cs], [n(X)|Ts]) :- \+ digit(C), !, lex(nil, [C|Cs], Ts).
lex(nil,  [C|Cs], Ts)        :-   identifierf(C), !, lex(i([C]), Cs, Ts).
lex(i(X), [C|Cs], Ts)        :-    identifier(C), !, put(X,C,A), lex(i(A), Cs, Ts).
lex(i(X), [C|Cs], [i(X)|Ts]) :- \+ identifier(C), !, lex(nil, [C|Cs], Ts).
lex(nil,  [C|Cs], [o(O)|Ts]) :- operator(C, O), !, lex(nil, Cs, Ts).
lex(nil,  Cs, [o(assign)|Ts]):- append([58, 61], CsT, Cs), !, lex(nil, CsT, Ts).

evaluate :-
    read_line_to_codes(user_input, Line),
    lex(nil, Line, Tok),
    write(Tok).


