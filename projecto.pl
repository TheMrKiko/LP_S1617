%-------------------------------------------------------------------------
%                          PROJECTO
%-------------------------------------------------------------------------
:- include('SUDOKU').
:- include('testes publicos/exemplos.pl').
%-------------------------------------------------------------------------
%           Predicados para a propagacao de mudancas
%-------------------------------------------------------------------------

tira_num_aux(Num, Puz, Pos, N_Puz) :-
    puzzle_ref(Puz, Pos, Cont),
    subtract(Cont, [Num], N_Cont),
    puzzle_muda_propaga(Puz, Pos, N_Cont, N_Puz).
% tira_num_aux(Num, Puz, Pos, N_Puz): N_Puz e' o puzzle resultante de
% tirar o numero Num da posicao Pos de Puz.

tira_num(Num, Puz, Posicoes, N_Puz) :-
    percorre_muda_Puz(Puz, tira_num_aux(Num), Posicoes, N_Puz).
% tira_num(Num, Puz, Posicoes, N_Puz): N_Puz e' o puzzle resultante de
% tirar o numero Num de todas as posicoes em Posicoes do puzzle Puz.

puzzle_muda_propaga(Puz, Pos, Cont, Puz) :-
    puzzle_ref(Puz, Pos, Cont_Atual),
    Cont = Cont_Atual, !.
% caso de paragem, se o que se for subtituir ja for o conteudo original.

puzzle_muda_propaga(Puz, Pos, Cont, N_Puz) :-
    Cont \= [_], !,
    puzzle_muda(Puz, Pos, Cont, N_Puz).
% se o conteudo novo nao for unitario, nao propaga.

puzzle_muda_propaga(Puz, Pos, Cont, N_Puz) :-
    Cont = [_],
    puzzle_muda(Puz, Pos, Cont, N_Puz_Int),
    posicoes_relacionadas(Pos, Posicoes),
    percorre_muda_Puz(N_Puz_Int, puzzle_subtrai_propaga(Cont), Posicoes, N_Puz).
% puzzle_muda_propaga(Puz, Pos, Cont, N_Puz): N_Puz e' o puzzle resultante
% de subtituir a posicao Pos por Cont. No caso de Cont ser unitario,
% propaga-se a mudanca pelas posicoes relacionadas

puzzle_subtrai_propaga(Cont_a_Subtrair, Puz, Pos, N_Puz) :-
    puzzle_ref(Puz, Pos, Cont_Atual),
    subtract(Cont_Atual, Cont_a_Subtrair, Cont_Novo),
    puzzle_muda_propaga(Puz, Pos, Cont_Novo, N_Puz).
% puzzle_subtrai_propaga(Cont, Puz, Pos, N_Puz): determina o conteudo novo da
% posicao Pos, com base no conteudo atual e no conteudo Cont que se pretende
% remover, do Puz.

%-------------------------------------------------------------------------
%           Predicados para a inicializacao de puzzles
%-------------------------------------------------------------------------

possibilidades(Pos, Puz, Poss) :-
    puzzle_ref(Puz, Pos, Cont),
    Cont \= [_], !,
    numeros(L),
    posicoes_relacionadas(Pos, Posicoes),
    percorre_muda_Puz(Puz, unitario, Posicoes, N_Puz),
    conteudos_posicoes(N_Puz, Posicoes, Conteudos),
    append(Conteudos, Conteudos_juntos),
    subtract(L, Conteudos_juntos, Poss).

possibilidades(Pos, Puz, Cont) :-
    puzzle_ref(Puz, Pos, Cont),
    Cont = [_].

% possibilidades(Pos, Puz, Poss): Poss e' a lista de numeros possiveis para
% a posicao Pos do puzzle Puz. Excepto se o conteudo de Pos nao e' unitario

unitario(Puz, Pos, Puz) :-
    puzzle_ref(Puz, Pos, Cont),
    length(Cont, 1), !.

unitario(Puz, Pos, N_Puz) :-
    puzzle_muda(Puz, Pos, [], N_Puz).
% unitario(Puz, Pos, N_Puz): N_Puz e' o puzzle resultante de subtituir Pos
% por [] se o conteudo nao for unitario.

inicializa_aux(Puz, Pos, N_Puz):-
    possibilidades(Pos, Puz, Poss),
    puzzle_muda_propaga(Puz, Pos, Poss, N_Puz).
% inicializa_aux(Puz,Pos,N_Puz): N_Puz e' o puzzle resultante de colocar em
% Pos a lista de numeros possiveis para la. Excepto se for unitario.

inicializa(Puz, N_Puz) :-
    todas_posicoes(Posicoes),
    percorre_muda_Puz(Puz, inicializa_aux, Posicoes, N_Puz).
% inicializa(Puz,N_Puz): N_Puz e' o resultado de inicializar Puz.

%-------------------------------------------------------------------------
%           Predicados para a inspeccao de puzzles
%-------------------------------------------------------------------------

so_aparece_uma_vez(Puz, Num, Posicoes, Pos_Num) :-
    so_aparece_uma_vez_aux(Puz, Num, Posicoes, [], Pos_Num).

so_aparece_uma_vez_aux(_, _, [], Pos_Num, Pos_Num) :-
    Pos_Num \= [].

so_aparece_uma_vez_aux(Puz, Num, [Pos|Resto], Ac, Pos_Num) :-
    puzzle_ref(Puz, Pos, Conteudo),
    member(Num, Conteudo), !,
    Ac = [],
    so_aparece_uma_vez_aux(Puz, Num, Resto, Pos, Pos_Num).

so_aparece_uma_vez_aux(Puz, Num, [_|Resto], Ac, Pos_Num) :-
    so_aparece_uma_vez_aux(Puz, Num, Resto, Ac, Pos_Num).

% so_aparece_uma_vez(Puz, Num, Posicoes, Pos_Num): Num so aparece numa posicao
% de Pos, a Pos_Num.

inspecciona_num(Posicoes, Puz, Num, N_Puz) :-
    so_aparece_uma_vez(Puz, Num, Posicoes, Pos_Num), !,
    puzzle_muda_propaga(Puz, Pos_Num, [Num], N_Puz).
inspecciona_num(_, Puz, _, Puz).
% inspecciona_num(Posicoes, Puz, Num, N_Puz): N_Puz e' o resultado de
% inspeccionar as Posicoes para o Num. Se Num so ocorrer numa dessas posicoes
% e se o seu conteudo nao for unitario, passa a ser. E propaga-se a mudanca.

inspecciona_grupo(Puz, Grupo, N_Puz) :-
    writeln('111'),
    dimensao(Nums),
    inspecciona_grupo_aux(Puz, Grupo, Nums, N_Puz).
% inspecciona_grupo(Puz, Grupo, N_Puz): N_Puz e' o resultado de inspeccionar o
% Puz para as posicoes em Grupo, verificando todos os numeros.

inspecciona_grupo_aux(Puz, _, 0, Puz) :- !.
inspecciona_grupo_aux(Puz, Grupo, Num, N_Puz) :-
    inspecciona_num(Grupo, Puz, Num, N_Puz_Int),
    Num_menos_1 is Num - 1,
    inspecciona_grupo_aux(N_Puz_Int, Grupo, Num_menos_1, N_Puz).
% inspecciona_grupo_aux(Puz, Grupo, Num, N_Puz): N_Puz e' o resultado de
% inspeccionar Puz para todas as Pos em Grupo para todos os numeros =< Num.

inspecciona(Puz, N_Puz) :-
    writeln('222'),
    grupos(Grupos),
    inspecciona_aux(Puz, Grupos, N_Puz).
% inspecciona(Puz, N_Puz): N_Puz e' o resultado de inspeccionar Puz.

inspecciona_aux(Puz, [], Puz) :- !.
inspecciona_aux(Puz, [Grupo|Resto], N_Puz) :-
    inspecciona_grupo(Puz, Grupo, N_Puz_Int),
    inspecciona_aux(N_Puz_Int, Resto, N_Puz).
% inspecciona_aux(Puz, Grupos, N_Puz): N_Puz e' o resultado de inspeccionar Puz
% para todos os Grupos, para todas os numeros possiveis.

%-------------------------------------------------------------------------
%           Predicados para a verificacao de solucoes
%-------------------------------------------------------------------------
grupo_correcto(Puz, Nums, Grupo) :-
    writeln('333'),
	conteudos_posicoes(Puz, Grupo, Conteudos),
	msort(Nums, Nums_sorted),
    append(Conteudos, Conteudos_app),
	msort(Conteudos_app, Conteudos_sorted),
	Nums_sorted = Conteudos_sorted.
% grupo_correcto(Puz, Nums, Grupo): o grupo de posicoes em Grupo, do Puz,
% contem todos os numeros sem repeticoes em Nums.

solucao(Puz) :-
	numeros(Nums),
	grupos(Grupos),
	solucao_aux(Puz, Nums, Grupos).
% solucao(Puz): todos os grupos de Puz tem todos os numeros possiveis, unicamente.

solucao_aux(_, _, []) :- !.
solucao_aux(Puz, Nums, [C|Resto]) :-
	grupo_correcto(Puz, Nums, C),
	solucao_aux(Puz, Nums, Resto).
% solucao_aux(Puz, Nums, Grupos): os grupos de posicoes de Puz em Grupos tem
% todos os Nums sem repeticoes.

%-------------------------------------------------------------------------
%                   	   Predicado resolve
%-------------------------------------------------------------------------
resolve(Puz, Sol) :-
	inicializa(Puz, Puz_Inic),
    inspecciona(Puz_Inic, Sol),
    escreve_puzzle(Sol),
    solucao(Sol).

resolve(Puz, Sol) :-
    inicializa(Puz, Puz_Inic),
    inspecciona(Puz_Inic, Puz_Inspec),
    pos_nao_uni(Puz_Inspec, Pos),
    puzzle_ref(Puz_Inspec, Pos, Cont),
    member(Num, Cont),
    puzzle_muda_propaga(Puz_Inspec, Pos, [Num], Sol),
    solucao(Sol), !.
% resolve(Puz, Sol): Sol e' a solucao do Puz resolvido.

pos_nao_uni(Puz, Pos) :-
    todas_posicoes(Todas_Posicoes),
    member(Pos, Todas_Posicoes),
    puzzle_ref(Puz, Pos, Cont),
    Cont \= [_].
% pos_nao_uni(Puz, Pos): Pos e' uma posicao nao unitaria de Puz.

%-------------------------------------------------------------------------
%                          FIM DO PROJECTO
%-------------------------------------------------------------------------
