junta([], L, L).
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2).

ant(X, Y) :- ad(X, Y).
ant(X, Z) :- ant(X, Y), ad(Y, Z).
ad(marge, bart).
ad(srB, marge).

leitura(Y) :- read(Y).