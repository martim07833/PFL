% ==============================================================================
% PROJETO
% ==============================================================================
% Este ficheiro contém o código do projeto
% Estuda os comentários marcados com [QUESTÕES] e [E SE...?].

% ==============================================================================
% 1. FACTOS (A BASE DO TABULEIRO)
% ==============================================================================

% [QUESTÕES]
% Porque é que isto são factos e não regras?
% R: Porque são dados estáticos. Não dependem de cálculo.

% [E SE...?]
% P: "Como farias para o tabuleiro ser 4x4 (ou ter apenas 4 cores)?"
% R: Bastava alterar esta lista 'colors' para ter apenas 4 elementos e ajustar
%    os factos 'edge_1' e 'edge_2' para os novos índices.
%    O resto do código (regras) é GENÉRICO e funcionaria igual.
% Define a lista de cores disponíveis para preencher o tabuleiro.
colors([green, yellow, blue, orange, white, black]).

% Definição das Bordas
% Índices das posições que formam a borda pequena.
edge_1([1, 2]).       % Borda Pequena
% Índices das posições que formam a borda grande.
edge_2([4, 5, 6]).    % Borda Grande

% ==============================================================================
% 2. AUXILIARES 
% ==============================================================================

% --- PERMUTAÇÃO (O GERADOR) ---
% [QUESTÕES]
% Explica isto em voz alta:
% "Este predicado gera todas as ordens possíveis das cores.
%  Ele usa recursão para ir tirando uma cor de cada vez (my_select)
%  e construindo uma nova lista."
% Caso base: a permutação da lista vazia é a lista vazia.
my_permutation([], []).
% Escolhe um elemento da lista e permuta recursivamente o resto.
my_permutation(List, [Head|Tail]) :-
    % Escolhe uma cor da lista original.
    my_select(Head, List, Rest),
    % Permuta o restante após remover a cor escolhida.
    my_permutation(Rest, Tail).

% --- SELEÇÃO ---
% [QUESTÕES]
% O que acontece aqui? (Conceito de Não-Determinismo)
% O 'my_select' é onde a magia do backtracking acontece.
% 1. Tenta tirar a cabeça (X).
% 2. Se falhar mais à frente, volta atrás e tenta tirar da cauda (Tail).
% Seleciona o primeiro elemento da lista, removendo-o do resultado.
my_select(X, [X|Tail], Tail).
% Seleciona X em qualquer posição posterior, preservando os anteriores.
my_select(X, [Head|Tail], [Head|Rest]) :-
    % Procura X mais à frente e mantém os elementos prévios.
    my_select(X, Tail, Rest).

% --- LISTAS & ÍNDICES ---

% [QUESTÕES]
% Porque é que fizeste o teu próprio nth1 (get_index)?
% R: Para não depender da library(lists) e garantir que controlo os índices (base 1).

% Retorna 1 se a cor estiver na primeira posição da lista.
get_index(Color, [Color|_], 1).  % Caso base: Encontrei na cabeça, índice é 1.
% Procura a cor na cauda e incrementa o índice ao regressar.
get_index(Color, [_|Tail], Index) :-
    % Busca a cor na parte restante da lista.
    get_index(Color, Tail, N),   % Recursão: Procura na cauda.
    % Soma 1 ao índice obtido na recursão.
    Index is N + 1.              % Quando voltar, soma 1 à contagem.

% [QUESTÕES]
% Como funciona este máximo?
% R: Percorre a lista mantendo um acumulador (CurrentMax).
% Inicia a procura do máximo usando o primeiro elemento como acumulador.
my_max_list([H|T], Max) :- find_max(T, H, Max).

% Quando chega ao fim da lista, devolve o máximo acumulado.
find_max([], Max, Max). % Cheguei ao fim, o acumulador é o resultado.
% Atualiza o máximo corrente se encontrar valor maior e continua.
find_max([H|T], CurrentMax, FinalMax) :-
    % Calcula o novo máximo entre o atual e o elemento H.
    (H > CurrentMax -> NewMax = H ; NewMax = CurrentMax), % If-Then-Else simples
    % Continua a verificar o resto da lista com o novo máximo.
    find_max(T, NewMax, FinalMax).

% --- POSICIONAMENTO RELATIVO ---

% [E SE...?]
% P: "O que faz esse append com underscores?"
% R: O append serve para partir a lista.
%    [_, [X,Y|_], _] significa: "Ignora o início, quero encontrar X seguido de Y, e ignora o fim".
% Verdadeiro se X surge imediatamente antes de Y no tabuleiro.
consecutive(X, Y, Board) :- append(_, [X, Y | _], Board).

% Verdadeiro se X e Y estão separados por exatamente uma posição.
one_space_apart(X, Y, Board) :- append(_, [X, _, Y | _], Board).

% ==============================================================================
% 3. REGRAS DO JOGO (CONSTRAINTS)
% ==============================================================================

% [QUESTÕES CRÍTICA] - O CUT (!)
% P: "O que acontece se tirares este '!' (Cut)?"
% R: O programa fica mais lento e pode dar erros lógicos subtis.
%    Se X for igual a Y, o 'ignore_self' tem sucesso. Sem o Cut, o Prolog
%    tentaria verificar as regras abaixo (next_to, etc.) mesmo que X fosse igual a Y,
%    o que é desperdício de tempo e, no caso de 'position', poderia falhar.
%    Este é um "Green Cut" (eficiência) que se comporta como "Red Cut" (lógica) neste contexto.
% Falha imediatamente quando compara a mesma cor, evitando regras redundantes.
ignore_self(X, X) :- !.

% 1. Anywhere
% Regra permissiva: aceita qualquer posição das cores.
anywhere(_, _).

% 2. Next To
% [QUESTÕES]
% Porque tens três linhas para o next_to?
% 1. Caso X=Y (ignora).
% 2. Caso X esteja antes de Y.
% 3. Caso Y esteja antes de X (Simetria). O 'next_to' não tem direção.
% Ignora a comparação se X e Y forem iguais.
next_to(X, Y, _) :- ignore_self(X, Y).
% Verdadeiro se X estiver imediatamente antes de Y.
next_to(X, Y, Board) :- consecutive(X, Y, Board).
% Verdadeiro se Y estiver imediatamente antes de X (simetria).
next_to(X, Y, Board) :- consecutive(Y, X, Board).

% 3. One Space
% Ignora a regra quando compara a mesma cor.
one_space(X, Y, _) :- ignore_self(X, Y).
% Verdadeiro se X e Y tiverem um espaço entre eles.
one_space(X, Y, Board) :- one_space_apart(X, Y, Board).
% Também considera a ordem inversa.
one_space(X, Y, Board) :- one_space_apart(Y, X, Board).

% 4. Position
% [QUESTÕES]
% Para que serve o 'var(X), !'?
% R: Defesa defensiva. Se a variável X não tiver valor (ex: erro no código),
%    isto impede que o 'get_index' rebente ou entre em loop infinito.
% Se X não estiver instanciado, corta e evita trabalho desnecessário.
position(X, _, _) :- var(X), !.
% Verifica se o índice da cor X pertence às posições permitidas.
position(X, ListOfPositions, Board) :-
    % Obtém o índice de X no tabuleiro.
    get_index(X, Board, Index),
    % [QUESTÕES] Porque usas member aqui?
    % R: Para verificar se o Índice onde a cor está (Index) pertence à lista de
    %    posições permitidas (ListOfPositions).
    % Confirma se o índice calculado está na lista de posições válidas.
    member(Index, ListOfPositions).

% 5. Same Edge
% Ignora a regra se X e Y forem a mesma cor.
same_edge(X, Y, _) :- ignore_self(X, Y).
% Verdadeiro se X e Y estiverem na mesma borda do tabuleiro.
same_edge(X, Y, Board) :-
        % Encontra o índice de X na configuração atual.
        get_index(X, Board, IX),
        % Encontra o índice de Y na configuração atual.
        get_index(Y, Board, IY),
        % Obtém as duas bordas para comparação.
        edge_1(E1), edge_2(E2),
        % Verifica se ambos estão em E1 OU ambos estão em E2.
        ( (member(IX, E1), member(IY, E1)) ;
            (member(IX, E2), member(IY, E2)) ).

% 6. Across
% Ignora a regra se comparar a mesma cor.
across(X, Y, _) :- ignore_self(X, Y).
% Verdadeiro se X e Y estiverem em bordas opostas.
across(X, Y, Board) :-
        % Índice de X no tabuleiro.
        get_index(X, Board, IX),
        % Índice de Y no tabuleiro.
        get_index(Y, Board, IY),
        % Obtém as listas das duas bordas.
        edge_1(G1), edge_2(G2),
        % Um na borda 1 e o outro na borda 2 (ou vice-versa).
        ( (member(IX, G1), member(IY, G2)) ;
            (member(IX, G2), member(IY, G1)) ).

% ==============================================================================
% 4. SOLUCIONADORES
% ==============================================================================

% --- SOLVE (GENERATE & TEST) ---
% [QUESTÕES]
% Explica o paradigma usado aqui.
% R: É "Generate and Test". Primeiro gero uma solução candidata (my_permutation),
%    e depois testo se cumpre todas as regras (satisfies_all).
%    Se o teste falhar, o Prolog faz Backtracking para o my_permutation e tenta outra.

% [E SE...?]
% P: "Como alterarias o solve para devolver o número de tentativas falhadas?"
% R: Em Prolog puro é difícil porque o backtracking apaga a memória.
%    Teria de usar um contador dinâmico (assert/retract) cada vez que entrasse
%    no 'satisfies_all' e falhasse.
% Gera um tabuleiro candidato e verifica se cumpre todas as restrições.
solve(Constraints, Board) :-
    % Obtém a lista de cores disponíveis.
    colors(Colors),
    % Gera uma permutação das cores como possível tabuleiro.
    my_permutation(Colors, Board),
    % Testa o tabuleiro contra todas as restrições fornecidas.
    satisfies_all(Constraints, Board).

% [QUESTÕES]
% O que faz o 'call(Constraint, Board)'?
% R: Metaprogramação. O 'Constraint' vem como `next_to(blue, red)`.
%    O `call` adiciona o `Board` como último argumento, transformando-o em
%    `next_to(blue, red, Board)` e executa-o.
% Caso base: nenhuma constraint por verificar.
satisfies_all([], _).
% Aplica a constraint atual ao tabuleiro e continua com as restantes.
satisfies_all([Constraint | Rest], Board) :-
    % Executa a constraint atual usando o tabuleiro.
    call(Constraint, Board),
    % Prossegue com as restantes constraints.
    satisfies_all(Rest, Board).

% --- BEST SCORE (OTIMIZAÇÃO) ---

% [E SE...?]
% P: "Como devolvias não só o Score, mas também o Tabuleiro correspondente?"
% R: No findall, em vez de recolher apenas 'S', recolheria um par 'S-Board'.
%    Ficaria: findall(S-Board, (...), AllScores).
%    Depois teria de adaptar o my_max_list para comparar pares baseando-se na chave (S).

% Calcula o melhor score possível para as restrições dadas.
best_score(Constraints, Score) :-
    % Obtém todas as cores possíveis.
    colors(Colors),
    % [QUESTÕES] Porque usas findall?
    % R: Porque preciso de ver TODAS as permutações possíveis para saber qual é a máxima.
    %    O findall isola o backtracking e devolve uma lista limpa com os resultados.
    % Gera todos os scores possíveis para cada tabuleiro.
    findall(S, (my_permutation(Colors, Board), calculate_score(Constraints, Board, S)), AllScores),
    % Seleciona o maior score obtido.
    my_max_list(AllScores, Score).

% Score é o negativo do número de restrições violadas.
calculate_score(Constraints, Board, Score) :-
    % Conta quantas restrições não são satisfeitas.
    count_violations(Constraints, Board, Violations),
    % Score é negativo para que menos violações signifique maior score.
    Score is -Violations.

% Caso base: sem restrições, não há violações.
count_violations([], _, 0).
% Conta 1 violação se a constraint falhar; 0 caso contrário.
count_violations([Constraint | Rest], Board, Violations) :-
    % [QUESTÕES] Explica esta seta (->).
    % R: É um If-Then-Else local. "Se call(Constraint) for verdade, então V=0, senão V=1".
    %    Uso isto para não criar pontos de escolha (backtracking) desnecessários na contagem.
    % Avalia a constraint e associa 0 (cumprida) ou 1 (falhada).
    ( call(Constraint, Board) -> V = 0 ; V = 1 ),
    % Continua a contagem para o resto das constraints.
    count_violations(Rest, Board, RestViolations),
    % Soma as violações acumuladas.
    Violations is V + RestViolations.

% ==============================================================================
% 5. EXEMPLOS
% ==============================================================================
example(1, [next_to(white, orange), next_to(black, black), across(yellow,orange), next_to(green, yellow), position(blue, [1,2,6]), across(yellow,blue) ]).
example(2, [across(white, yellow), position(black, [1,4]), position(yellow, [1,5]), next_to(green, blue), same_edge(blue,yellow), one_space(orange, black) ]).
example(3, [across(white, yellow), position(black, [1,4]), position(yellow, [1,5]), same_edge(green, black), same_edge(blue, yellow), one_space(orange, black) ]).


% ==============================================================================        ============================================================================== %
% ============================================================================== TESTES ============================================================================== %
% ==============================================================================        ============================================================================== %
% ==============================================================================        ============================================================================== %



% ==============================================================================
% PROLOG 2025 - EXERCÍCIOS IMPORTANTES
% ==============================================================================

% Common Elements

shared_genres(Title1, Title2, CommonGenres) :-
    book(Title1, _AuthID, _Year, _Pages, Genres1),
    book(Title2, _AuthID, _Year, _Pages, Genres2),
    common_elements(Genres1, Genres2, CommonGenres).

common_elements([], _L, []).
common_elements([H|T], L, [H|R]):-
    member(H, L), !,
    common_elements(T, L, R).
common_elements([_|T], L, R):-
    common_elements(T, L, R).

% Acumuladores

% Ex2

ingredient_amount_cont(Ingredient, Grams, TotalCost) :-
    ingredient(Ingredient, CpG),
    TotalCost is CpG * Grams.

dish_profit(Dish, Profit) :-
    dish(Dish, SellPrice, IngredientGrams),
    auxGet_Cost(IngredientGrams, 0, Cost),
    Profit is SellPrice - Cost.


auxGet_Cost([], Profit, Profit).
auxGet_Cost([Ingredient-Grams | Is], Acc, Profit):-
    ingredient_amount_cont(Ingredient, Grams, Price),
    NewAcc is Acc + Price,
    auxGet_Cost(Is, NewAcc, Profit).

% Max Element + Backtracking

% Ex4 - sem findall
% Encontra o prato mais caro usando negação por falha.
% Estratégia: um prato é o mais caro se não existe nenhum outro prato com preço maior.
most_expensive_dish(Dish, Price) :-
    % Unifica Dish e Price com um prato e seu preço da base de dados
    dish(Dish, Price, _),
    % \+ = negação por falha (NOT Provable)
    % Verifica que NÃO EXISTE (nenhum) prato com preço superior
    \+ (dish(_, OtherPrice, _), OtherPrice > Price).
    % dish(_, OtherPrice, _) - procura qualquer prato com preço OtherPrice
    % OtherPrice > Price - verifica se esse preço é maior
    % Se nenhum prato satisfaz ambas as condições, a negação sucede

% Ex4.1 - com findall
% Encontra o prato mais caro recolhendo todos os preços.
% Estratégia: encontrar o máximo de todos os preços e depois unificar com pratos que têm esse preço.
most_expensive_dish2(Dish, Price) :-
    % findall(+Template, +Goal, -List)
    % Recolhe TODOS os preços de TODOS os pratos numa lista
    findall(P, dish(_, P, _), Prices),
    % sort(+List, -Sorted)
    % Ordena a lista de preços em ordem crescente (predicado built-in)
    sort(Prices, SortedPrices),
    % append(?List1, ?List2, ?List3)
    % Obtém o último elemento: append(_, [Price], SortedPrices)
    % Significa: existe uma lista que concatenada com [Price] dá SortedPrices
    append(_, [Price], SortedPrices),
    % Unifica Dish com cada prato que tem esse preço máximo
    % Permite backtracking para retornar múltiplos pratos em caso de empate
    dish(Dish, Price, _).


% -----  Exame 2024  ----- 

    Consider the following knowledge base regarding books and authors.
        %author(AuthorID, Name, YearOfBirth, CountryOfBirth).
        %book(Title, AuthorID, YearOfRelease, Pages, Genres).
    
    Implement book_author(?Title, ?Author), which associates a book title with the name of its author.
        book_author(Title, Author):-
            author(AuthID, Author, _YoB, _CoB),
            book(Title, AuthID, _YoP, _Pages, _Genres).
        
    Implement multi_genre_book(?Title), which unifies Title with the title of a book that has multiple genres.
        multi_genre_book(Title):- book(Title, _AuthID, _Year, _Pages, [_One, _Two | _Rest]).
        multi_genre_book(Title):-
            book(Title, _AuthID, _Year, _Pages, Genres),
            length(Genres, Len), % length is a built-in predicate
            Len > 1.
        multi_genre_book(Title):-
            book(Title, _AuthID, _Year, _Pages, Genres),
            member(A, Genres), % member is a built-in predicate
            member(B, Genres), % member is a built-in predicate
            A \= B.

    Implement shared_genres(?Title1, ?Title2, CommonGenres), which receives two book titles as arguments and returns on the third argument a list containing the genres that are common to both books. Any order of the shared genres is valid.
        shared_genres(Title1, Title2, CommonGenres):-
            book(Title1, _ID1, _Year1, _Pages1, Genres1),
            book(Title2, _ID2, _Year2, _Pages2, Genres2),
            common_elements(Genres1, Genres2, CommonGenres).
        common_elements([], _L, []).
        common_elements([H|T], L, [H|R]):-
            member(H, L), !,
            common_elements(T, L, R).
        common_elements([_|T], L, R):-
            common_elements(T, L, R).

    The Jaccard coefficient, also known as intersection over union IoU, is a similarity measurement between two sets, determined by the division between the intersection (number of common elements between the two sets) and the union (total number of different elements in both sets). Implement the similarity(?Title1, ?Title2, ?Similarity) predicate, which determines the Jaccard coefficient between the two books received as first two arguments, considering the genres of each book as the measure of similarity
        similarity(Title1, Title2, Similarity):-
            shared_genres(Title1, Title2, Intersection),
            book(Title1, _ID1, _Year1, _Pages1, Genres1),
            book(Title2, _ID2, _Year2, _Pages2, Genres2),
            union(Genres1, Genres2, Union),
            length(Intersection, LI),
            length(Union, LU),
            Similarity is LI / LU.
        union(Set1, Set2, UnionSet):-
            append(Set1, Set2, All),
            sort(All, UnionSet).

    The Passionate Fans of Literature PFL Book Club instituted a Secret Santa for Christmas. In a draw of luck, each member of the club was assigned another member for whom to buy a gift. Being a book club, everyone bought a book, and the full information was registered after the PFL Christmas Dinner in the following database:
        % gives_gift_to(Giver, Gift, Receiver)
    Implement circle_size(+Person, ?Size), which unifies the second argument with the number of people who form the circle of gifts that includes the person received as first argument.
        circle_size(Person, Size):-
            collect([Person], People),
            length(People, Size).
        collect( [H|T], People):-
            gives_gift_to(H, _, N),
            \+ member(N, [H|T]), !,
            collect( [N,H|T], People).
        collect(People, People).

    Implement largest_circle(?People), which unifies People with the list of people belonging to the largest circle in the book club. The order of the people in the list is not important. In case there is more than one circle with the largest dimension, the predicate should succeed more than once. The order of the results is not important.
        :-use_module(library(lists)).
        largest_circle(People):-
            all_people(Everyone),
            setof(Size-Person-Sorted, Persons^(member(Person, Everyone), collect([Person], Persons), sort(Persons, Sorted),
        length(Sorted, Size)), Triples),
            last(Triples, MaxSize-_-_),
            setof(Persons, P^member(MaxSize-P-Persons, Triples), LargestGroups),
            member(People, LargestGroups).
        all_people(List):-
            findall(X, (gives_gift_to(X, _, _) ; gives_gift_to(_, _, X)), Temp),
            sort(Temp, List).

    Implement dec2bin(+Dec, -BinList, +N), which converts a non-negative integer number Dec into a list of bits representing that number, using exactly N bits. If the number of bits is insufficient to represent the number, the predicate should fail. If the number to convert is negative, the predicate should fail.
        dec2bin(Dec, List, N):-
            Dec >= 0,
            dec2bin(Dec, [], List, N).
        dec2bin(0, List, List, 0):- !.
        dec2bin(Dec, Acc, List, N):-
            N > 0,
            Bit is Dec mod 2,
            Next is Dec div 2,
            N1 is N - 1,
            dec2bin(Next, [Bit|Acc], List, N1).

    Implement initialize(+DecNumber, -Bits, +Padding, -List), which receives a decimal number and number of bits in which to represent it, as well as Padding - the number of zeroes to place on each side of the resulting binary representation - returning in List the resulting list of bits.
        initialize(Dec, N, Padd, List):-
            dec2bin(Dec, Mid, N),
            dec2bin(0, Side, Padd),
            append([Side, Mid, Side], List).

    Implement print_generation(+List), which prints to the terminal a text representation of a list of bits, representing 0 as a dot ('.'), 1 as a capital M ()'M'), and separating each byte (set of 8 bits) with a pipe ('|').
        print_line(0, []):- !,
            write('|'), nl.
        print_line(_, []):- !, nl.
        print_line(0, Bits):- !,
            write('|'),
            print_line(8, Bits).
        print_line(N, [Bit|Bits]):-
            N1 is N-1,
            translate(Bit, Char),
            put_char(Char),
            print_line(N1, Bits).
        translate(0, '.').
        translate(1, 'M').
        print_generation(L):-
            write('|'),
            print_line(8, L).

    The rules for updating each cell from one generation to the next consider the state of the left neighbor, the cell itself, and the right neighbor. For each of the eight possible combinations, the rule(?Config, ?State) predicate gives the resulting cell state, where the first argument is a three-bit configuration represented as a compound term Left-Self-Right, and the second argument is the resulting state bit. Implement update_rule(+Rule), which receives a number between 0 and 255, and changes the knowledge base so that exactly eight rule/2 facts exist. This predicate should always succeed, except in case it receives a value outside the expected range of values
        update_rule(N):-
            \+ (dec2bin(N, Bits, 8)), !, fail.
        update_rule(N):-
            dec2bin(N, Bits, 8),
            abolish(rule/2),
            between(0, 1, FirstBit),
            between(0, 1, SecondBit),
            between(0, 1, ThirdBit),
            Index is 8 - (FirstBit * 4 + SecondBit * 2 + ThirdBit),
            nth1(Index, Bits, Bit),
            assert( rule(FirstBit-SecondBit-ThirdBit, Bit) ),
            fail.
        update_rule(_).

    Implement next_gen(+Previous, Next), which receives a list of binary values and computes the next generation, applying the existing rules to each position. Missing neighbors (for the first and last elements) are assumed to be zeroes.
        next_gen(Gen0, Gen1):-
            apply_rules(0, Gen0, Gen1).
        apply_rules(Left, [Self], [New]):-
            rule(Left-Self-0, New).
        apply_rules(Left, [Self, Right | Rest], [New | Tail]):-
            rule(Left-Self-Right, New),
            apply_rules(Self, [Right|Rest], Tail).
    
    Implement play(+DecNumber, Bits, Padding, Rule, N) which receives the input values for the initialize/4 predicate (DecNumber, Bits, and Padding), the input value for the update_rule/1 predicate (Rule), and N, the number of generations to simulate. This predicate should orchestrate the calls to existing predicates and print the first N generations to the terminal. Note that a call to play/5 with N  1 results in printing the initial generation only
        play(Init, N, Padd, Rule, Gens):-
            initialize(Init, N, Padd, Gen0),
            update_rule(Rule),
        play_gens(Gens, Gen0).
            play_gens(1, Gen):- !,
        print_generation(Gen).
            play_gens(N, Gen0):-
            N1 is N -1,
            print_generation(Gen0),
            next_gen(Gen0, Gen1),
            play_gens(N1, Gen1).



% ==============================================================================        ============================================================================== %
% ============================================================================ CONTEUDOS ============================================================================= %
% ==============================================================================        ============================================================================== %
% ==============================================================================        ============================================================================== %

%-----  INTRODUCTION TO PROLOG  -----

    -----  Disjunction  -----
    Disjunction can also be expressed with the ; operator. The disjunction operator (;) should be used sparingly. Always use parentheses to clarify

        parent(X, Y):- father(X, Y). % X is a parent of Y if X is the father of Y
        parent(X, Y):- mother(X, Y). % X is a parent of Y if X is the mother of Y
        % is equivalent to
        parent(X, Y):- father(X, Y) ; mother(X, Y).

    -----  Logic Table  -----
        Implication     A :- B      B -> A
        Conjunction     A , B       A ∧ B
        Disjunction     A ; B       A V B

    -----  Terms  -----
    Everything in Prolog is a term, which can be
            • Constant
            • Variable
            • Compound term

    -----  Constants  -----
    Constants represent elementary objects

    Numbers
            • Integers (e.g., 4, -8) (bases other than decimal can also be used, e.g., 8’755)
            • Floats (e.g., 1.5, -1.6) (also supports exponent, e.g., 23.4E-2)

    Atoms
            • Start with lower-case letter (e.g., john_doe, johnSmith42)
            • String within single quotes (e.g., "John Doe", "John Smith 42")

    -----  Variables  -----
    Variables act as placeholders for arbitrary terms
            • Start with a capital letter (e.g., Variable1)
            • Start with an underscore (e.g., _Var2)
            • Single underscore (_) (anonymous variable)

    Variables are universally instantiated in logic programs
            plus(0, S, S).          % 0 is the neutral element of addition
            mult(1, V, V).          % 1 is the neutral element of multiplication
            human(Homer).           % everything is human
            father(homer, Bart)     % homer is the father of everything
            grandfather(X, Y):- father(X, Z), parent(Z, Y).

    Variables occurring only in the body of a rule can be seen as existentially quantified. We need to be careful when using variables with facts.

    -----  Compound Terms  -----
    Atoms, numbers and variables are building blocks for compound terms Compound terms are comprised of a functor and arguments (which are terms)
        
        The functor is characterized by its name (an atom) and arity (the number of arguments), usually represented as name/arity
        
        E.g., point/2 represents a functor named point with two arguments
            point(4, 2) is a possible instance of point/2, and so is point(foo, point(3, bar))

    ----- Queries -----
    Computations in Prolog start with a question, which has two possible answers:
        Yes (possibly with answer substitution - variable binding)
        No
    
    The attempt to prove the question right/wrong (is it a consequence of the program?) produces the computations
            | ?- male(homer).               yes
            | ?- father(homer, bart).       yes
            | ?- female(marge).             yes
            | ?- father(marge, bart).       no

    ----- Variables in Queries -----
    Queries can include variables - Variables are existentially quantified in queries. A variable starting with an underscore is a "don't care"
            | ?- father(X, bart).           X = homer ?
                                            yes
            | ?- father(_X, bart).          yes
            | ?- male(_).                   yes
            | ?- male(X).                   X = homer ?
                                            yes
            | ?- male(X).                   X = homer ? ;
                                            X = bart ? n
                                            no
    If satisfied with the answer, just hit enter .If you want another answer, type "n", "no" or ";"

    ----- Variables and Compound Queries -----
    Queries can be more complex, combining goals. Variables are used to glue together the different goals - Underscore alone (_) is the exception
            | ?- male(X), parent(X, bart).          X = homer ? ;
                                                    no
            | ?- male(_X), parent(_X, bart).        yes
            | ?- male(_X), parent(Y, bart).         Y = homer ? ;
                                                    Y = marge ? ;
                                                    Y = homer ? ;
                                                    Y = marge ? ;
                                                    no

    ----- Closed Word Assumption -----
    Assumption that everything that is true is known to be true (i.e., is represented as a clause in the program). Therefore, everything that cannot be deduced from the clauses in the program is assumed to be false.
            | ?- male(donald).      no
    Requires attention to make sure everything we want to deduce can be deduced from the program clauses

    ----- Horn Clauses -----
    Everything in Prolog is expressed as a Horn Clause
            Rules are complete horn clauses (head :- body)
            male(homer):- true.     <=>     male(homer).
    
    Facts are horn clauses where the body is always true (just the head)
            parent(X, Y):- father(X, Y).        <=>     father(X, Y) => parent(X, Y)

    Queries are horn clauses without a head (just the body)
            | ?- father(X, bart).

    ----- Predicates -----
    A predicate is a set of clauses for the same functor
        Clauses are either facts or rules - parent is a predicate with two clauses:
                parent(marge, bart).
                parent(homer, bart).
    
    Functors with the same name but different arity refer to different predicates
                father(X):- father(X, Y).       % X is a father
                                                % if X is the father of some Y

    ----- How Prolog Works -----
        Top to bottom - The order of clauses is important
        Left to right - In rules, prove sub-goals in left-to-right order
        With backtracking - If a sub-goal fails, go back to previous decision point



% -----  UNIFICATION AND EXECUTION MODEL  ----- 

    ----- Substitution -----
    Recall everything in Prolog is a term. Terms can be either:
            • Ground - there are no variables in the term (completely instantiated)
            • Unground - there are variables in the term
    
    Unification is how Prolog matches two terms. Two terms are unifiable if
            • they are the same, or
            • they can be the same after variable substitution

    A substitution 𝜃 is a set of pairs Xi = ti where
            • Xi is a variable
            • ti is a term
            • Xi ≠ Xj for all i ≠ j
            • Xi does not occur in any tj, for all i and j

    To apply a substitution 𝜃 to a term T (T𝜃) is to replace in T all occurrences of Xi for ti, for all pairs Xi=ti in 𝜃
            T = father(X, bart)
            𝜃 = {X=homer}
            T𝜃 = father(homer, bart)
    
    A is said to be an instance of B if there is a substitution 𝜃 such that A = B𝜃
            father(homer, bart) is an instance of father(X, bart)

    A term T is a common instance of T1 and T2 if there are substitutions 𝜃1 and 𝜃2 such that T = T1𝜃1 and T = T2𝜃2
            parent(homer, bart) is a common instance of parent(X, bart) and parent(homer, Y)


    A term G is more general than term T if T is an instance of G but G is not an instance of T
            parent(X, bart) is more general than parent(homer, bart)
    
    A term V is a variant of a term T if they can be converted into one another by a simple variable renaming
            parent(Y, bart) is a variant of parent(X, bart)

    ----- Unification -----
    Given two atomic sentences, p and _q, a unification algorithm returns a substitution 𝜃 (the most general unifier) that makes them identical (or fails if such substitution does not exist):
            Unify(p, _q) = 𝜃 where p𝜃 = _q𝜃
    𝜃 is said to be the (most general) unifier of the two sentences
    
    The most general unifier (MGU) is the one that compromises the variables as little as possible - the respective instance is the most general
            Unify( parent(X, bart), parent(Y, Z) ) produces 𝜃 = { Y=X, Z=bart }

    Example: Unification of f(X, a) and f(b, Y)
            1. Push f(X,a) = f(b,Y)
            2. Pop → same functor f, arity 2 → push X = b and a = Y
            3. Pop a = Y → Y is variable and doesn’t occur in a → add Y = a to θ
            4. Pop X = b → X is variable and doesn’t occur in b → add X = b to θ
            5. Stack empty → return θ = {X = b, Y = a}

    Unification in Practice
    Both terms are constants: the terms unify if they are the same.
    One of the terms is a variable: it is instantiated to the other term.
    If both terms are variables, they are bound to each other.
    Two compound terms unify if:
            They have the same functor and arity
            All the corresponding arguments unify
            All substitutions are compatible

    ----- Computation -----
    Program P composed of Clauses - Clauses are universally quantified logical sentences
            A:- B1, ..., Bk, k >= 0
            A and Bi are goals
    
    Computation of a Logic Program P:
        Find an instance of a given query Q logically deducible from P
        Query is an existentially quantified conjunction
                A1, ..., An, n > 0
                Ai are goals
        Goal: Atom or compound term

    Given a program P and an initial query Q, computation terminates:
        With success - (an instance of) Q was proven (Multiple successful computations (solutions) may exist)
        Without success - Q cannot be proven
    
    Computation may not terminate (no result) .Non-termination comes from recursive rules that may not end - Avoid left-recursive rules:

            ancestor(X, Y) :- ancestor(X, Z), parent(Z, Y).

            married(homer, marge).
            ...
            married(X, Y):- married(Y, X).

    Resolvent is a conjunctive question (query) with the set of goals still to be processed

    Trace is the evolution of the computation (sequence of resolvents) with information regarding:
        Selected goal
        Rule selected for reduction
        Associated substitution

    Reduction is the replacement, in the resolvent, of a goal G with the body of a clause whose head unifies with G

    ----- Abstract Interpreter -----
    Abstract interpreter algorithm, given program P and query Q:

    Let resolvent be Q
    While resolvent is not empty do
        1. Choose a goal A from resolvent
        2. Choose a renamed clause B :- B1, ..., Bn from P such that A and B unify with an MGU 𝜃 (exit if no such goal and clause exist)
        3. Remove A from resolvent and add B1, ..., Bn to resolvent
        4. Apply 𝜃 to resolvent and to Q
    If resolvent is empty, return Q; else return failure

    ----- Execution Model -----
    An implementation of Logic Programming needs to instantiate the abstract interpreter, making choices that influence how the computation is performed
        Choice of goal from resolvent
        Choice of clause
        Add goal(s) to resolvent
    Different languages / implementations may make different choices to implement the abstract interpreter

    Prolog’s implementation of the abstract interpreter
        • Choice of goal from resolvent: left to right
                Choice is arbitrary, does not affect computation (logical meaning, not operational)
        • Choice of clause: top to bottom with backtracking
                Choice affects computation
        • Add goal(s) to resolvent: at the beginning
                Results in a depth-first search
                If it were to be added to the end, it would result in a breadth-first search (assuming leftmost goal is chosen next)

    ----- Search Trees -----
    A search tree contains all possible search paths
        Root: Query Q
        Nodes: resolvents, with selected goal
        Edges: one edge for each clause in P whose head unifies with the selected goal in the source node - Includes substitution from the unification
        Leaves: success nodes, if empty resolvent; or fail nodes
        Paths from root to leaves: computation of Q using P

    Example:

    a :- b, c.
    b :- d.
    b :- e.
    c.
    e.
    | ?- a, e.

    Search Tree:

    (a, e) --> (b, c, e) --> (d, c, e) --> fail (d doesnt get resolved)
                         --> (e, c, e) --> (c, e) --> (e) --> success!

    It is independent of the clause selection criteria (it contains all alternatives). There can be different search trees for the same query and program, depending on the goal selection criteria. The number of success nodes is the same in all trees.
    
    Contains all answers
        it is named search tree because a concrete interpreter needs a strategy to traverse the tree searching for solutions
        Depth-first search, breadth-first search, parallel search, ...



% -----  RECURSION AND ARITHMETIC  ----- 

    ----- Recursion -----
    Recursion is based on the inductive proof One or more base clauses and one or more recursion clauses

            ancestor(X, Y):-            % X is an ancestor of Y
                parent(X, Y).           % if X is a parent of Y

            ancestor(X, Y):-            % X is an ancestor of Y
                parent(X, Z),           % if X is a parent of Z
                ancestor(Z, Y).         % and Z is an ancestor of Y
    
    The order of clauses and goals may influence performance, or even cause infinite computations

            sumN(0, 0).                 % Base clause
            sumN(N, Sum):- N > 0,       % Guard - make sure we don’t
                N1 is N-1,              % have infinite recursion
                sumN(N1, Sum1),         % Recursive call
                Sum is Sum1 + N.

    ----- Tail Recursion -----
    Tail Recursion can increase efficiency. Add a new argument to the predicate: the accumulator. Make the recursive call the last call
            sumN(N, Sum):- sumN(N, Sum, 0).         % Encapsulate
            sumN(0, Sum, Sum).                      % Base case – the result is 
            sumN(N, Sum, Acc):- N > 0,              % in the accumulator
            N1 is N-1,
            Acc1 is Acc + N,
            sumN(N1, Sum, Acc1).                    % Recursive call is now the last sub-goal
    To increase efficiency, we actually need to add a cut in the base clause

    ----- Arithmetic -----
    Arithmetic expressions are not evaluated immediately
        Example: A = 4+2 unifies A with the term +(4, 2), not the value 6
    The _is_ predicate can be used to evaluate an arithmetic expression - The right-side of is needs to be instantiated

    Examples:
            | ?- A = 4 + 2.             A = 4+2 ?
                                        yes
            | ?- B is 4 + 2.            B = 6 ?
                                        yes
            | ?- 6 is 4 + 2.            yes
            | ?- 4+2 is 4+2.            No
            | ?- C is 4+B.              ! Instation error in argument 2 of (is)/2
                                        ! goal: _419 is 4+_427

    Arithmetic expressions can be compared for (in)equality
        Expr1 =:= Expr2 evaluates both expressions and if they are equal
        Expr1 =\= Expr2 evaluates both expressions and if they are different
        Comparison
            E1 < E2     E1 > E2     E1 =< E2    E1 >= E2
    Prolog can also compare and order terms
            T1 @< T2    T1 @> T2    T1 @=< T2   T1 @>= T2
        Term1 == Term2 verifies whether the two terms are literally identical
        Term1 \== Term2 checks if the two terms are not literally identical

    There are several functions available
        X + Y, X - Y, X * Y, X / Y (float quotient)
        X // Y is the integer quotient, truncated towards 0
        X div Y is the integer quotient (rounded down)
        X rem Y is integer remainder: X - Y * (X // Y)
        X mod Y is integer remainder: X - Y * (X div Y)  

    Examples:
            | ?- A is 5 // 2.           A = 2 ?
                                        yes
            | ?- A is .5 // 2.          A = -2 ?
                                        yes
            | ?- A is 5 div 2.          A = 2 ?
                                        yes
            | ?- A is -5 div 2.         A = -3 ?
                                        yes
            | ?- A is 5 rem 2.          A = 1 ?
                                        yes
            | ?- A is -5 rem 2.         A = -1 ?
                                        yes
            | ?- A is 5 mod 2.          A = 1 ?
                                        yes
            | ?- A is -5 mod 2.         A = 1 ?
                                        yes
    

-----  LISTS  ----- {

    ----- Lists -----
    Lists are the quintessential data structure in Prolog. Empty list represented as [ ]. Elements separated by commas within square brackets
        [a, b, c]
        [4, 8, 15, 16, 23, 42]
    Lists elements can be anything, including other lists
        [ 1, [a, b, v], g, [2, [D, y], 3], 4 ]
    
    The internal representation uses the . functor and two arguments - the head and tail of the list
        [1, 2, 3] = .(1, .(2, .(3, []) ) ).
        | ?- A = .(1, .(2, .(3, []) ) ).        A = [1, 2, 3] ?
                                                yes                          

    Strings are a representation of lists of character ASCII codes
        | ?- A = "Hello".                       A = [72, 101, 108, 108, 111] ?
                                                yes

    Easily separate the head of the list from the rest of the list - the head of the list can separate more than one element
        [ H | T ]               % where T is a list with the remaining elements of the list
        [ 4 ] = [ 4 | [ ] ]     % tail of list with one element is empty list
        [4, 8, 15, 16, 23, 42] = [4 | [8, 15, 16, 23, 42] ]
        [4, 8, 15, 16, 23, 42] = [ 4, 8 | [ 15, 16, 23, 42] ]

    Definition of what is a list
        An empty list                               is_list( [ ] ).
        A list construct where tail is a list       is_list( [H|T] ):- is_list(T).

    ----- The Mystical Four List Functions - SLAM! -----
    sort(+L, -L') : sorts list L and put it in list L'. removes duplicates
    Recursive implementation:
        /do this before test/

    length(?L, ?N) : puts length of l in n
    Recursive implementation:
        length( [ ], 0 ).
        length( [_|T], L ):-
            length(T, L1),
            L is L1+1.

    append(?L1, ?L2, ?L) : the same as haskell ++ l1 is the first list, l2 is the second, l is the output list
    Recursive implementation:
        append( [ ], L2, L2 ).
        append( [H|T], L2, [H|T3] ):-
            append(T, L2, T3).

    member(?E,?L) : is element e in list l? can be used as for to iterate through the list (make e a variable)
    Recursive implementation
        member( X, [X|_] ).
        member( X, [_|T] ):-
            member(X, T).
        
        memberchk( X, [X|_] ).
        memberchk( X, [Y|T] ):-
            X \= Y,
            memberchk(X, T).

    +   -   Variável instanciada
    -   -   Variável que não pode estar instanciada (vai passar a estar unificada)
    ?   -   Variável híbrida (pode estar instanciada ou não)

    ----- Lists library -----

    nth0(?Pos, ?List, ?Elem) / nth1(?Pos, ?List, ?Elem)
    nth0(?Pos, ?List, ?Elem, ?Rest) / nth1(?Pos, ?List, ?Elem, ?Rest)
    Predicate                   Indexing starts at      Example
    nth0(Index, List, Elem)     0 (zero-based)          nth0(0, [a,b,c], X). % X = a
    nth1(Index, List, Elem)     1 (one-based)           nth1(1, [a,b,c], X). % X = a

    Form                            Meaning                         Example
    nth0(I, L, E)                   Get element E at position I     nth0(2,[a,b,c,d],X). % X=c
    nth1(I, L, E)                   Same using 1-based index        nth1(4,[a,b,c,d],X). % X=d
    nth0(I, L, E)                   with E known Find index I       nth0(I,[a,b,c],b). % I=1
    nth0(Index, List, Elem, Rest)   Get element and list without    nth0(2,[a,b,c,d],X,R).
                                    it (delete elem.)               % X=c, R=[a,b,d]
    nth0(Index, List, Elem, Rest)   Add an element                  nth0(2,X,c,[a,b,d]).
                                                                    % X=[a,b,c,d]
    nth0(Index, List, Elem, Rest)   Replace an element              nth0(1,[a,b,c],X,R),
                                                                    nth0(1,S,f,R). 
                                                                    % X=b, R=[a,c], S=[a,f,c]

    select(?X, ?XList, ?Y, ?YList)
        finds an occurrence of X in XList, replaces it with Y, and produces YList

    delete(+List, +ToDel, -R)
    delete(+List, +ToDel, +Count, -R)
        Deletes Count occurrences of ToDel in List, result R
    
    last(?Init, ?Last, ?List)
        Last element of List and the rest in Init

    segment(?List, ?Segment)
        succeed when Segment is a contiguous subsequence of List.
    
    sublist(+List, ?Part, ?Before, ?Length, ?After)
        extract a contiguous Part of List with Length size and Before/After pre/suffix

    append(+ListOfLists, -List)
        concate of Haskell

    reverse(?List, ?Reversed)

    rotate_list(+Amount, ?List, ?Rotated)
        cyclically shifts (rotates) List by Amount number of positions

    transpose(?Matrix, ?Transposed)
        converts rows into columns (and vice-versa)
    
    remove_dups(+List, ?PrunedList)
    
    permutation(?List, ?Permutation)
        List permutations, with backtracking

    sumlist(+ListOfNumbers, ?Sum)
    max_member(?Max, +List)
    min_member(?Min, +List)
    max_member(:Comp, ?Max, +List)
        Comp is a comparison predicate of arity 2 used to compare elements
    min_member(:Comp, ?Min, +List)

    maplist(:Pred, +L) / maplist(:Pr, +L1, ?L2) / maplist(:Pr, +L1, ?L2, ?L3)
        Applies predicate to each element / map / zipWith

    map_product(:Pred, +Xs, +Ys, ?List)
        Cartesian product

    scanlist(:Pred, +Xs, ?Start, ?Final)
        foldl
    
    cumlist(:Pred, +Xs, ?Start, ?List)
        Similar to accumulate in python

    some(:Pred, +List)
        any
    
    include(:P, +X, ?L) / include(:P, +X, +Y, ?L) / include(:P, +X, +Y, +Z, ?L)
        filter / P(x, y) succeeds, L ⊆ X / P(x, y, z) succeeds, L ⊆ X

    exclude(:P, +X, ?L) / exclude(:P, +X,+Y, ?L) / exclude(:P, +X,+Y,+Z, ?L)
        not include

    group(:Pred, +List, ?Front, ?Back)
        Group until predicate fails, splitting the list at that point

}

-----  NON-LOGICAL FEATURES  ----- {

    ----- Cuts -----
    backtracking in Prolog can lead to some inefficiency. Branches that to no feasible solution are still explored. Soultion: cut (!)

    Always succeeds as a goal (can be ignored in a declarative reading). Binds Prolog to all choices made since the parent goal unified with the clause where the cut is. This prunes all clauses for the same predicate below the one where the cut is and all alternative solutions to the goals left of the cut in the clause, but does not prune the goals to the right of the cut in the clause. these can produce several solutions via backtracking, however,backtracking to the cut fails and causes backtracking to the last choice point.

    Example:
        a(X, Y) :- b(X), !, b(Y)            
        a(3, 4).                                      
        b(2).
        b(3).                       
        
        | ?- a(X, Y).
        X = 2,
        Y = 2 ? ;
        X = 2,
        Y = 3 ? ;
        no

    sumN with cut:
        sumN(N, Sum) :- sumN(N, Sum, 0).
        sumN(0, Sum, Sum) :- !.

        sumN(N, Sum, Acc) :- N > 0,
                             N1 is N-1
                             Acc1 is Acc + N,
                             sumN(N1, Sum, Acc1).

    ----- Red Cuts Vs. Green Cuts -----
    Red cut is one that influences the results. If we remove the cut, the results will be different.
    a(A, B) :- b(A), !, b(B).           a(A, B) :- b(A), b(B)
    a(3, 4).                            a(3, 4).
    b(2).                               b(2).
    b(3).                               b(3).

    | ?- a(X, Y).                       | ?- a(X, Y).
    X = 2,                              X = 2,
    Y = 2 ? ;                           Y = 2 ? ;
    X = 2,                              X = 2,
    Y = 3 ? ;                           Y = 3 ? ;
    no                                  X = 3,
                                        Y = 2 ? ;
                                        X = 3,
                                        Y = 3 ? ;
                                        X = 3,
                                        Y = 4 ? ;
                                        no

    Green cut is one that does not influence results, but is used to incresase efficiency. If we remove the cuts, the results will be the same, but Prolog will explore branches that wont lead to any possible solution.
    classify(BMI, "low weight"):- BMI < 18.5, !.
    classify(BMI, "normal weight"):- BMI >= 18.5, BMI < 25, !.
    classify(BMI, "excessive weight"):- BMI >= 25, BMI < 30, !.
    classify(BMI, "obesity"):- BMI >= 30, !.

    ----- Negation as Failure -----
    Negation can be attained by using a cut.
    not(X) :- X, !, fail
    not(_X).
    Fail always fails. The cut is necessary to ensure the second clause is not reached when backtracking.

    Negation should be used with ground terms (no variables in the goal), or "strange" results may occur. Example: determine if a man is not a father
            not_a_father(X):- not(parent(X, _)), male(X).

    Works well with instantiated values, but what about with a variable?
            not_a_father(bart).         not_a_father(X).
            yes                         no

    Change the order of the goals so that variables in the negated goal are ground (possibly instantiated by other goals in the clause)
            not_a_father(X):- male(X), not(parent(X, _)).

    ----- Conditional as Failure -----
    We can attain a conditional execution by using two clauses with a mutually exclusive condition verification
        pred_ite(If, Then, _Else):- If, Then.
        pred_ite(If, _Then, Else):- not(If), Else.

    Conditional execution can also be attained by using a cut
        if_then_else(If, Then, _Else):- If, !, Then.
        if_then_else(_If, _Then, Else):- Else.

    ----- Input/Output -----
    Input / Output is based on streams, used either for reading or writing, in text (characters and terms) or binary (bytes) mode. At any one time there is one current input stream and one current output stream (by default the user’s terminal). I/O predicates operate on the corresponding current stream. All predicates support additional parameter (as the first one) specifying the stream to read from / write to
    
    Input and output cannot be undone, but variable binding (from input predicates) is undone when backtracking

    • read/1 reads a term (by default, from the standard input)
            Input needs to end with a period (spans multiple lines)
            If a compound term is being read, input must match term being read
            Use unnamed variables (_X)
    • write/1 writes a term
    • nl/0 prints a new line
    • get_char obtains a single character
    • get_code obtains the ASCII code of a single character
    • put_char prints a single character
    • put_code prints a single character given its ASCII code
    • char_code(?Atom, ?Code) allows converting between character and corresponding ASCII code
    • get_byte and put_byte read and write binary data
    • peek_char, peek_code and peek_byte obtain a single character / code / byte without consuming it from the input stream
    • format prints terms with specified formatting options
    • skip_line skips any input until the end of the line - It is OS independent

    ----- File Input/Output -----
    There are some useful predicates to work with files
    • see/1 opens a file for reading -The file is used for reading instead of the standard input
    • seen/0 closes the file that was opened for reading
    • tell/1 opens a file for writing - The file is used for writing instead of the standard output
    • told/0 closes the file that was opened for writing
    Other predicates exist to open, manage and close streams

    ----- Libraries -----
    Several directives can be used to import files
        use_module(library(lib_name)) % for libraries or modules
        consult(file_to_load)
        [file_to_load]
        ensure_loaded(file_to_load)
        include(file_to_include)

    Repeat
    repeat always succeeds. Can be used to repeat some portion of code until it succeeds. It may be useful to use a cut after reaching the condition to break the cycle, to avoid undesired backtracking
        read_value(X):-
            repeat,
            write("write hello"),
            read(X),
            X = hello.
        
    Between
    between(+Lower, +Upper, ?Number) can be used both to test and generate integers between given bounds. Necessary to include the between library
        | ?- between(1, 6, 4).
        yes
        | ?- between(1, 6, 9).
        no
        | ?- between(1, 3, X).
        X = 1 ? ;
        X = 2 ? ;
        X = 3 ? ;
        no
    Hint: you can use repeat together with between to test for valid coordinate input in the practical assignment

    Random
    Random library provides several predicates for generating random numbers
        maybe / maybe(+Probability)
        random(+Lower, +Upper, -Value)
        random_member(-Element, +List)
        random_select(?Element, ?List, ?Rest)
        random_permutation(?List, ?Permutation)

}

-----  GRAPHS AND TREES  ----- {

    ----- Collecting Solutions -----
    Prolog provides three predicates to obtain multiple solutions to a query: findall, bagof, and setof. They allow systematic collection of answers to any goal. The template is similar to all three predicates. Returns the list List of all instances of Term such that Goal is provable. Goal specifies a goal to be called. List is a list of terms
        findall/bagof/setof(?Term, :Goal, -List).

    ----- findall -----
    findall finds all solutions, including repetitions if present. If there are no solutions, an empty list is returned
        | ?- findall(Child, parent(homer, Child), Children).
        Children = [lisa, bart, maggie] ? ;
        no
        | ?- findall(Parent, parent(Parent, _Child), List).
        List = [homer,homer,homer,marge,marge,marge] ? ;
        no
        | ?- findall(Child, parent(bart, Child), List).
        List = [] ? ;
        no

    We can use a conjunctive goal (parentheses are required)
        | ?- findall(C, ( parent(homer, C), female(C) ), Daughters).
        Daughters = [lisa, maggie] ? ;
        no
    We can obtain more than one variable using a compound term
        | ?- findall(Parent-Child, parent(Parent, Child), L).
        L = [homer-lisa, homer-bart, homer-maggie, marge-lisa, ...] ? ;
        no
    If all we want is a count, we can use anything
        | ?- findall(_, parent(homer, _C), _L), length(_L, N).
        N = 3 ? ;
        no

    ----- bagof -----
    bagof has similar behavior, but results are grouped by variables appearing in Goal but not in the search Term
        | ?- findall(Child, parent(Parent, Child), Children).
        Children = [lisa, bart, maggie, lisa, bart, maggie] ? ;
        no
        | ?- bagof(Child, parent(Parent, Child), Children).
        Parent = homer, Children = [lisa, bart, maggie] ? ;
        Parent = marge, Children = [lisa, bart, maggie] ? ;
        no
    While findall returns an empty list if there are no results, bagof fails
        | ?- findall(Child, parent(bart, Child), L).
        L = [] ? ;
        no
        | ?- bagof(Child, parent(bart, Child), L).
        no
    
    We can direct bagof to ignore additional variables in Goal by using existential quantifiers: Var^Goal. If all variables appearing in Goal but not in the search Term are existentially quantified, then bagof behaves like findall
        | ?- bagof(Child, parent(Parent, Child), Children).
        Parent = homer, Children = [lisa, bart, maggie] ? ;
        Parent = marge, Children = [lisa, bart, maggie] ? ;
        no
        | ?- bagof(Child, Parent^parent(Parent, Child), Children).
        Children = [lisa, bart, maggie, lisa, bart, maggie] ? ;
        no

    ----- setof -----
    setof has similar behavior to bagof, but results are ordered and without repetitions.
        | ?- bagof(Child, parent(Parent, Child), Children).
        Parent = homer, Children = [lisa, bart, maggie] ? ;
        Parent = marge, Children = [lisa, bart, maggie] ? ;
        no
        | ?- setof(Child, parent(Parent, Child), Children).
        Parent = homer, Children = [bart, lisa, maggie] ? ;
        Parent = marge, Children = [bart, lisa, maggie] ? ;
        no
    Existential quantifiers can also be used with setof, with the same effect as with bagof (results will remain ordered and without repeats). If all variables in Goal but not in search Term are existentially quantified, then setof behaves like findall followed by sort
        | ?- bagof(Child, Parent^parent(Parent, Child), Children).
        Children = [lisa, bart, maggie, lisa, bart, maggie] ? ;
        no
        | ?- setof(Child, Parent^parent(Parent, Child), Children).
        Children = [bart, lisa, maggie] ? ;
        no

    ----- Graphs and Searches -----
    Graphs can be represented as the connections between nodes - set of facts representing [directed] edges. Searching for a possible connection between nodes is made easy by Prolog’s standard depth-first search mechanism
        connected(porto, lisbon).           connects_dfs(S, F):- 
        connected(lisbon, madrid).              connected(S, F).
        connected(lisbon, paris).           connects_dfs(S, F):-
        connected(lisbon, porto).               connected(S, N),
        connected(madrid, paris).               connects_dfs(N, F).
        connected(madrid, lisbon).          | ?- connects_dfs(porto, madrid).
        connected(paris, madrid).           yes
        connected(paris, lisbon).           | ?- connects_dfs(madrid, porto).
    Adapted solution with an accumulator to avoid loops
        connects_dfs(S, F):-
            connects_dfs(S, F, [S]).
        connects_dfs(F, F, _Path).
        connects_dfs(S, F, T):-
            connected(S, N),
            not( memberchk(N, T) ),
            connects_dfs(N, F, [N|T]).
        | ?- connects_dfs(madrid, porto).
        yes
    
    We can also easily create a BFS solution using findall
        connects_bfs(S, F):-
            connects_bfs([S], F, []).
        connects_bfs([F|_], F, _V).
        connects_bfs([S|R], F, V):-
            findall(
                N,
                ( connected(S, N),
                not(memberchk(N, V)),
                not(memberchk(N, [S|R]))),
                L),
            append(R, L, NR),
            connects_bfs(NR, F, [S|V]).

    ----- Binary trees -----
    A binary tree can be recursively defined using node elements. Empty node represented as null. Other nodes as node(Value, Left, Right)
        binary_tree(null).
        binary_tree( node(Value, Left, Right) ):-
            binary_tree(Left),
            binary_tree(Right).

    Tree operations are easily implemented from this definition
    Check if value is a member of the tree
        tree_member(Val, node(Val, _L, _R) ).
        tree_member(Val, node(V, L, _R) ):-
            [Val < V,] tree_member(Val, L).
        tree_member(Val, node(V, _L, R) ):-
            [Val > V,] tree_member(Val, R).
    List all tree elements (in-order traversal)s
        tree_list( null, [] ).
        tree_list( node(Val, L, R), List ):-
            tree_list(L, Left),
            tree_list(R, Right),
            append(Left, [Val|Right], List).
    Verify if tree is ordered
        tree_is_ordered(Tree):-
            tree_list(Tree, List),
            sort(List, List).
    Insert an element into the tree
        tree_insert( null, V, node(V, null, null) ).
        tree_insert( node(V, L, R), V, node(V, L, R) ).
        tree_insert( node(V, L, R), Val, node(V, NL, R) ):-
            Val < V, tree_insert( L, Val, NL).
        tree_insert( node(V, L, R), Val, node(V, L, NR) ):-
            Val > V, tree_insert( R, Val, NR).
    Determine the height of the tree
        tree_height( null, 0).
        tree_height( node(Val, L, R), H):-
            tree_height(L, HL),
            tree_height(R, HR),
            H is 1 + max(HL, HR).
    Check whether the tree is balanced
        tree_is_balanced( null ).
        tree_is_balanced( node(Val, L, R) ):-
            tree_is_balanced(L),
            tree_is_balanced(R),
            tree_height(L, HL),
            tree_height(R, HR),
            abs(HL-HR) =< 1.
    
    ----- Games and Puzzles -----
    A generic [abstract] solver to one-person games/puzzles
        initial(InitialState).
        
        final(State):- winning_condition(State).
        
        move(OldState, NewState):- valid_move(OldState, NewState).
        
        play(CurrSt, Path, Path):- final(CurrSt), !.
        play(CurrSt, Path, States):- move(CurrSt, Next),
                not( member(Next, Path) ),
                play(Next, [Next|Path], States).
        
        play:- initial(Init),
                play(Init, [Init], States),
                reverse(States, Plays),
                write(Plays).

    Example: fill a 5-gallon jug with 4 gallons of water, using the 5-gallon jug and a 3-gallon jug
        initial(0-0). % Jug5-Jug3
        final(4-_).
        move(_-S, 5-S). % fill jug 1
        move(F-_, F-3). % fill jug 2
        move(_-S, 0-S). % empty jug 1
        move(F-_, F-0). % empty jug 2
        move(F-S, NF-NS):- NF is max(0, F+S-3), NS is min(3, F+S). % 1->2
        move(F-S, NF-NS):- NF is min(5, F+S), NS is max(0, F+S-5). % 2->1

    Shortest Path
    To find the smallest set of plays we just need to find all paths and select the shortest one - Easily accomplished using setof
        play:-  initial(Init),
                setof(
                    Length-Path,
                    ( play(Init, [Init], Path),
                    length(Path, Length) ),
                    [_ShortestLength-States|_]
                ),
                reverse(States, Path),
                write(Path).

}

-----  META-PROGRAMMING AND OPERATORS  ----- {

    ----- Meta-Programming -----
    Prolog has some meta-logical predicates for type checking
        integer(A)      A is an integer
        float(A)        A is a floating point number
        number(A)       A is a number (integer or float)
        atom(A)         A is an atom
        atomic(A)       A is an atom or a number
        compound(A)     A is a compound term
        var(A)          A is a variable (it is not instantiated)
        nonvar(A)       A is an atom, a number or a compound term
        ground(A)       A is nonvar, and all substructures are nonvar

    These predicates can be very useful to implement different versions of predicates depending on variable instantiation.
        grandparent(X, Y):- nonvar(Y), !, parent(Z, Y), parent(X, Z).
        grandparent(X, Y):- parent(X, Z), parent(Z, Y).
    We can think of an implementation of the sum/3 predicate that tests for instantiation, using a more appropriate definition in each case
        sum(A, B, S):- number(A), number(B), !, S is A + B.
        sum(A, B, S):- number(A), number(S), !, B is S - A.
        sum(A, B, S):- number(B), number(S), !, A is S - B.
    Other predicates allow access to terms and their arguments / to construct new terms
    functor(+Term, ?Name, ?Arity) or functor(?Term, +Name, +Arity)
        If Term is instantiated, returns the name and arity of the term
        If Term is not instantiated, creates a new term with given name and arity
        process_term(Term, Result) :-
            functor(Term, F, _Arity),
            dispatch(F, Term, Result).
        
        dispatch(sum, sum(A,B), R) :- R is A + B.
        dispatch(diff, diff(A,B), R) :- R is A - B.
        dispatch(neg, neg(X), R) :- R is -X.
        dispatch(const, const(C), C).
        
        | ?- process_term(sum(2,3), R).
        R = 5.

    arg(+Index, +Term, ?Arg) -Given an index and a term, instantiates Arg with the argument in the Nth position (index starts in 1)
        | ?- arg(2, paretn(homer, bart), Arg).
        Arg = bart ?
        yes
    
    +Term =.. ?[Name | Args] or ?Term =.. +[Name | Args] - Given a term, returns a list with the name and arguments of the term. Given a proper list, creates a new term with name and arguments as specified by the contents of the list
        | ?- paretn(homer, bart) =.. List.
        List = [parent,homer,bart] ?
        yes
        | ?- Term =.. [parent, homer, bart].
        Term = parent(homer,bart) ?
        yes

    The call/1 predicate calls (executes) a given goal
        | ?- C = write("Hello World!"), call(C).
        Hello World!
        C = write("Hello World!") ?
        yes
        | ?- C = write("Hello World!"), c.
        Hello World!
        C = write("Hello World!") ?
        yes
        | ?- G =.. [write, "Hi there!"], G.
        Hi there!
        G = write("Hi there!") ?
        yes
    In this example, C is a meta-variable - it repreents a callable goal. callable/1 verifies if a term is callable. call/1 can be used with up to 255 arguments, in which case the first term is extended with the remaning arguments. The first argument has to instantiated, this has a similar effect to using univ to construct the term to call.

    ----- Operators -----
    Prolog allows for the definition of new operators. We can easily change the way we write programs:
        homer likes marge.
        marge likes homer.
        homer and marge parented bart.
        homer and marge parented lisa.
    Operators are characterized by precedence and associativity.
    Precedence determines which operation is executed first. The lower, the more priority the operator has. Precedence in Prolog is given by a number between 1 and 1200 - Multiplication has precedence level 400 and Addition has precedence level 500
    Associativity determines how to associate operations. Division is left-associative, while ^ is right associative.

    The op/3 predicate can be used to specify new operators.
        op(+Precedence, +Type, +Name).
        Precedence is a number between 1 and 1200
        Type defines the type and associativity of the operator.
            Prefix - fx or fy           f defines the position of the operator
            Postfix - xf or yf          x and y represent the operands
            Infix - xfx, xfy or yfx     x means non-associative and y means side-associate

    Built-in operators:
        :- op(1200, xfx, [ :-, --> ]).
        :- op(1200, fx, [:-, ?- ]).
        :- op(1150, fx, [mode, public, dynamic, volatile, discontiguous, multifile, block, meta_predicate, initialization]).
        :- op(1100, xfy, [;, do]).
        :- op(1050, xfy, [ -> ]).
        :- op(1000, xfy, [ ',' ]).
        :- op(900, fy, [\+, spy, nospy]).
        :- op(700, xfx, [=, \=, is, =.., ==, \==, @<, @>, @=<, @>=, =:=, =\=, <, >, =<, >=]).
        :- op(550, xfy, [:]).
        :- op(500, yfx, [+, -, \, /\, \/]).
        :- op(400, yfx, [*, /, //, div, mod, rem, <.<, >>]). (no point betwen <)
        :- op(200, xfx, [**]).
        :- op(200, xfy, [ ^ ]).
        :- op(200, fy, [+, -, \]).

    Example:
        :-op(400, xfx, parented).
        :-op(380, xfy, and).
        
        X and Y parented Z:-
            bagof(S,
                ( parent(X, S),
                parent(Y, S), X@<Y), L),
                as_list(L, Z).
        
        as_list([A, B|T], A and R):- !,
            as_list([B|T], R).
        as_list([A], A).

    ----- Computaional Models -----
    Emulate DFAs/NFAs (rules - initial(qi). / final(qf). / delta(q1, 0/1, q2).)
        accept(Str):-
            initial(State),
            accept(Str, State).
        accept([], State):-
            final(State).
        accept([S|Ss], State):-
            delta(State, S, NState),
            accept(Ss, NState)

    Emulate PDAs (rules - initial(qi). / final(qf). / delta(q1, A, B, q2, C). - A, B/C)
        accept(Str):- initial(State), accept(Str, State, []).
        accept([], State, []):- final(State).
        accept([S|Ss], State, Stack):-
            delta(State, S, Stack, NewState, NewStack),
            accept(Ss, NewState, NewStack).

    Emulate TM (rules - initial(qi). / final(qf). / delta(q1, L, [1/0|R], q2, [1/0|L], R).)
        tm(Str):- initial(State),
            append(Str, [empty], StrEmpty),
            tm([empty], StrEmpty, State).
        tm(Left, [S|Right], State):-
            delta(State, Left, [S|Right], NewState, NewLeft, NewRight),!,
            tm(NewLeft, NewRight, NewState).
        tm(_, _, State):- final(State).

    Emulate CFGs (rules - S --> empty / S --> X / S --> XSX)
        accept(Str):- ss(Str).
        ss([]).
        ss([X]).
        ss([X|SX]):- append(S, [X], SX), ss(S).
    The definition of CFGs can be simplified using DCGs (Definite Clause Grammars). It uses a syntax similar to the specification of grammar rules. It can be used both to recognize and to generate strings.
        pal --> [].
        pal --> [_].
        pal --> [S], pal, [S].
    Verifications can be made as extensions to the grammar rules
        palb --> [].
        palb --> [S], {[S] = "0"; [S]="1"}.
        palb --> [S], palb, [S], {[S] = "0"; [S]="1"}.
    More complex rules can be used
        expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
        expr(X) --> term(X).
        term(Z) --> num(X), "*", term(Y), {Z is X * Y}.
        term(Z) --> num(Z).
        num(X) --> [D], num(R), {"0"=<D, D=<"9", X is (D-"0")*10 + R}.
        num(X) --> [D], {"0"=<D, D=<"9", X is D-"0"}.

    ----- Incomplete Data Structures -----
    Incomplete data structures increase efficiency by allowing ‘partial’ or ‘incomplete’ structures to be specified and incrementally constructed during runtime. This is achieved by maintaining a free variable as the final element of the structure, as opposed to a constant (such as [] for lists or null). Changes to the incomplete structure can be made by [partially] instantiating the ending variable, thus not requiring the use of an extra output argument.

    Implementation of a dictionary using incomplete lists. When Key is present, Value is verified/returned. When Key is not present, the new Key-Value pair is added to the dictionary
        lookup(Key, [ Key-Value | Dic ], Value).
        lookup(Key, [ K-V | Dic ], Value):-
            Key \= K,
            lookup(Key, Dic, Value).
    Dictionary implemented with incomplete binary search tree
        lookup(Key, dtnode(Key-Value, _L, _R), Value).
        lookup(Key, dtnode(K-_V, L, _R), Value):-
            Key < K, lookup(Key, L, Value).
        lookup(Key, dtnode(K-_V, _L, R), Value):-
            Key > K, lookup(Key, R, Value).

    Difference Lists
    While lists are widely used, some common operations may not be very efficient, as is the case of appending two lists. Linear on the size of the first list. Idea: increase efficiency by ‘also keeping a pointer to the end of the list’. This is accomplished by using difference lists. We can use any symbol to separate the two parts of the difference list. With this representation, we can have an incomplete list (when the second list is not instantiated)
        X = [1, 2, 3]
        X = [1, 2, 3, 4, 5, 6]\[4, 5, 6]
        X = [1, 2, 3, a, b, c]\[a, b, c]
        X = [1, 2, 3]\[]
        X = [1, 2, 3 | T]\T
    We can now append two (difference) lists in constant time. To append X\Y with Z\W, simply unify Y with Z. Note that the two lists must be compatible - the tail of the first list must either be uninstantiated or be equal to the second list
        append_dl(X\Y, Y\W, X\W).
        | ?- append_dl( [a, b, c | Y ]\Y, [d, e, f | W]\W, A).
        Y=[d,e,f|W]
        A=[a,b,c,d,e,f|W]\W

    ----- Statistics ----- Theres no way they ask anything about this

    ----- SICStus Libraries -----
    aggregate - library provides operators for SQL-like queries. Results can be aggregated using sum, count, min, max, ...

    clpfd - library provides one of the best constraint programming solvers and library for integers. Very good for puzzles, and combinatorial optimization problems
        schedule(Ss, End):-
            length(Ss, 7), domain(Ss, 1, 30),
            length(Es, 7), domain(Es, 1, 50),
            buildTasks(Ss, [16,6,13,7,5,18,4], Es, [2,9,3,7,10,1,11], Tasks),
            maximum(End, Es),
            cumulative(Tasks, [limit(13)]),
            labeling([minimize(End)], [End|Ss]).
        buildTasks([], [], [], [], []).
        buildTasks([S|Ss], [D|Ds], [E|Es], [C|Cs], [task(S, D, E, C, 0)|Ts]):-
            buildTasks(Ss, Ds, Es, Cs, Ts).

}

-----  PRATICAL CLASS 2 - Recursion  ----- {

    -----  1.- Prolog and backtracking  -----
    r(a, b).        p(b, c).
    r(a, d).        p(b, d).
    r(b, a).        p(c, c).
    r(a, c).        p(d, e).
        i. r(X, Y), p(Y, Z).
        ii. p(Y, Y), r(X, Y).
        iii. r(X, Y), p(Y, Y).
    b) How many times does Prolog backtrack from the second to the first goal before producing the first answer for each of the queries? 
        i. 0 backtrack (r(a, b) -> p(b, c))
        ii. 0 backtrack (p(c, c) -> r(a, b))
        iii. 3 backtracks (r(a, b) -> p(b, c) / r(a, b) -> p(b, d) / r(a, b) -> p(c, c) / r(a, b) -> p(d, e))

    -----  2.- backtracking and Search tree  -----
    Search Tree for all solutions, caling trace. pairs(X, Y).:
        pairs(X, Y) :- d(X), q(Y).
        pairs(X, X) :- u(X).
        u(1).
        d(2).
        d(4).
        q(4).
        q(16).
    pairs(X, Y) --> X = 2 --> Y = 4     (2, 4)
                          --> Y = 16    (2, 16)
                --> X = 4 --> Y = 4     (4, 4)
                          --> Y = 16    (4, 16)
    pairs(X, X) --> X = 1               (1, 1)

    -----  4.- Recursion  -----
    a) factorial(+N, -F)
        factorial(0,1).
        factorial(N,F):-
            N >= 1,
            N1 is N-1,
            factorial(N1,F1),
            F is F1 * N.
    b) sum_rec(+N, -Sum)
        sum_rec(0, 0).
        sum_rec(N, Sum) :-
            N > 0,
            N1 is N-1,
            sum_rec(N1, Sum1),
            Sum is Sum1+N.
    c) pow_rec(+X, +Y, -P)
        pow_rec(_, 0, 1).
        pow_rec(X, Y, P) :-
            Y > 0,
            Y1 is Y - 1,
            pow_rec(X, Y1, P1),
            P is P1 * X.
    d) square_rec(+N, -S)
        square_rec(N, S) :- square_acc(N, 1, 0, S).
        square_acc(0, _, Acc, Acc).
        square_acc(N, Odd, Acc, S) :-
            N > 0,
            Acc1 is Acc + Odd,
            Odd1 is Odd + 2,
            N1 is N - 1,
            square_acc(N1, Odd1, Acc1, S).
    e) fibonacci(+N, -F).
        fibonacci(0,0).
        fibonacci(1,1).
        fibonacci(N,F):-
            N >= 2,
            N1 is N-1,
            fibonacci(N1,F1),
            N2 is N1-1,
            fibonacci(N2,F2),
            F is F1 + F2.
    f) colatz(+N, -S)
        collatz(1, 0).
        collatz(N, S) :-
            N > 1,
            0 is N mod 2,
            N1 is N // 2,
            collatz(N1, S1),
            S is S1 + 1.
        collatz(N, S) :-
            N > 1,
            1 is N mod 2,
            N1 is 3*N + 1,
            collatz(N1, S1),
            S is S1 + 1.
    g) is_prime(+X)
        is_prime(2).
        is_prime(X) :-
            X > 2,
            X mod 2 =\= 0,
            check_prime(X, X - 1). 
        check_prime(_, 1).
        check_prime(X, D) :-
            D > 1,
            X mod D =\= 0,
            D1 is D - 1,
            check_prime(X, D1).

    -----  5.- Tail Recursion  -----
    a) factorial_tailrec(+N, -F)
        factorial_tailrec(N,F):-
            factorial_tailrec_aux(N,1,F).  
        factorial_tailrec_aux(0,Acc,Acc).
        factorial_tailrec_aux(N,Acc,F):-
            N >= 1,
            N1 is N - 1,
            Acc1 is Acc * N,
            factorial_tailrec_aux(N1,Acc1,F).
    b) sum_tailrec(+N, -Sum)
        sum_tailrec(N, Sum) :-
            sum_tailrec_aux(N, 0, Sum).
        sum_tailrec_aux(0, Acc, Acc).
        sum_tailrec_aux(N, Acc, Sum) :-
            N > 0,
            N1 is N - 1,
            Acc1 is Acc + N,
            sum_tailrec_aux(N1, Acc1, Sum).
    c) pow_tailrec(+X, +Y, -P)
        pow_tailrec(X, Y, P) :-
            pow_tailrec_aux(X, Y, 1, P).
        pow_tailrec_aux(_, 0, Acc, Acc).
        pow_tailrec_aux(X, Y, Acc, P) :-
            Y > 0,
            Acc1 is Acc * X,
            Y1 is Y - 1,
            pow_tailrec_aux(X, Y1, Acc1, P).
    e) fibonacci_tailrec(+N, ?F).
        fibonacci_tailrec(0,0).
        fibonacci_tailrec(N,F):-
            fibonacci_tailrec_aux(N,0,1,F).
        fibonacci_tailrec_aux(1,_,F,F).
        fibonacci_tailrec_aux(N,F1,F2,F):-
            N >= 2,
            N1 is N-1,
            F3 is F1 + F2,
            fibonacci_tailrec_aux(N1,F2,F3,F).

    -----  6.- Greatest Common Divisor and Least Common Multiple  -----
    a) gcd(+X, +Y, -G)
        gcd(X, 0, X) :- X > 0.
        gcd(X, Y, G) :-
            Y > 0,
            R is X mod Y,
            gcd(Y, R, G).
    b) lcm(+X, +Y, -M)
        lcm(X, Y, M) :-
            X > 0,
            Y > 0,
            gcd(X, Y, G),
            M is X * Y // G.

    -----  7.- Family Relations Revisited  -----
    a) ancestor_of(?X, ?Y)
        ancestor_of(X, Y) :- parent(X, Y).
        ancestor_of(X, Y) :- parent(X, Z), ancestor_of(Z, Y).
    b) descendant_of(?X, ?Y)
        descendant_of(X, Y) :- parent(Y, X).
        descendant_of(X, Y) :- parent(Y, Z), descendant_of(X, Z).
    c) marriage_years(?X, ?Y, -Years)
        marriage_years(X, Y, Years) :-
            married(X, Y, MarriedYear),
            divorced(X, Y, DivorcedYear),
            Years is DivorcedYear - MarriedYear.
    e) 
        i) before(+X, +Y)
            before(X, Y) :- X < Y.
        ii) older(?X, ?Y, ?Older)
            older(X, Y, X) :- born(X, DX), born(Y, DY), before(DX, DY).
            older(X, Y, Y) :- born(X, DX), born(Y, DY), before(DY, DX).
        iii) oldest(?X)
            oldest(X) :- born(X, DX), \+ (born(Y, DY), before(DY, DX)).
    
    -----  8.-9.-  ----- se tiver tempo (e espaço) faço da outra e desta
    
}

-----  PRATICAL CLASS 3 - Lists  ----- {

    -----  1.- Lists  -----
    a) | ?- [a | [b, c, d] ] = [a, b, c, d].
        yes
    b) | ?- [a | b, c, d ] = [a, b, c, d].
        sintactic error, tail needs to be inside []
    c) | ?- [a | [b | [c, d] ] ] = [a, b, c, d].
        yes
    d) | ?- [H|T] = [pfl, lbaw, fsi, ipc].
        H = pfl, T = [lbaw, fsi, ipc]
    e) | ?- [H|T] = [lbaw, ltw].
        H = lbaw, T = [ltw]
    f) | ?- [H|T] = [leic].
        H = leic, T = []
    g) | ?- [H|T] = [].
        no, H needs to be an element, cant be empty list
    h) | ?- [H|T] = [leic, [pfl, ipc, lbaw, fsi] ].
        H = leic, T = [[pfl, ipc, lbaw, fsi]]
    i) | ?- [H|T] = [leic, Two].
        H = leic, T = [Two]
    j) | ?- [Inst, feup] = [gram, LEIC].
        Inst = gram, LEIC = feup
    k) | ?- [One, Two | Tail] = [1, 2, 3, 4].
        One = 1, Two = 2, Tail = [3,4]
    l) | ?- [One, Two | Tail] = [leic | Rest].
        One = leic, Rest = [Two | Tail]

}


% ==============================================================================
% 5. CHEAT SHEET
% ==============================================================================

% ------------------------------------------------------------------------------
% A. AGREGAÇÃO (FINDALL, BAGOF, SETOF) - Lecture 7
% ------------------------------------------------------------------------------

% 1. FINDALL: Junta tudo, com repetidos, devolve [] se não houver nada.
%    Ex: findall(X, book(X, ...), L).
demo_findall(Template, Goal, List) :- 
    findall(Template, Goal, List).

% 2. SETOF: Ordena, remove duplicados, FALHA se não houver soluções.
%    CUIDADO: Se houver variáveis livres no Goal, ele cria uma lista para cada valor!
%    Ex: setof(Title, book(Title, AuthID, ...), L). -> Gera uma lista por cada AuthID.
demo_setof_simple(Template, Goal, List) :- 
    setof(Template, Goal, List).

% 3. O QUANTIFICADOR EXISTENCIAL (^): "Ignora esta variável para agrupar"
%    Essencial para o SETOF/BAGOF devolverem SÓ UMA lista grande.
%    Lê-se: "Existe um AuthorID tal que..."
demo_setof_grouped(List) :-
    % Quero todos os livros, independentemente do autor.
    setof(Title, AuthorID^book(Title, AuthorID, _, _, _), List).

% ------------------------------------------------------------------------------
% B. LISTAS MANUAIS (SEM BIBLIOTECAS) - Lecture 5
% ------------------------------------------------------------------------------

% B1. BASICS
my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.

my_reverse(L, R) :- my_reverse_acc(L, [], R).
my_reverse_acc([], A, A).
my_reverse_acc([H|T], A, R) :- my_reverse_acc(T, [H|A], R).

my_nth1(1, [H|_], H) :- !.
my_nth1(N, [_|T], X) :- N > 1, N1 is N - 1, my_nth1(N1, T, X).

% B2. FILTRAR (FILTER / INCLUDE)
% Mantém apenas elementos que satisfazem uma condição.
% Ex: my_filter(Lista, ListaPares).
my_filter([], []).
my_filter([H|T], [H|R]) :-
    % COLOCAR AQUI A CONDIÇÃO (Ex: H > 0, ou uma chamada a predicado)
    check_condition(H), !, 
    my_filter(T, R).
my_filter([_|T], R) :-
    my_filter(T, R).
% Predicado auxiliar genérico para o exemplo acima
check_condition(X) :- integer(X), X mod 2 =:= 0. % Exemplo: é par

% B3. MAPEAR (MAP / TRANSFORM)
% Aplica uma transformação a todos os elementos.
% Ex: [1,2] -> [2,4].
my_map([], []).
my_map([H|T], [NewH|NewT]) :-
    % COLOCAR AQUI A TRANSFORMAÇÃO
    transform(H, NewH), 
    my_map(T, NewT).
transform(X, Y) :- Y is X * 2. % Exemplo: duplicar

% B4. ACUMULAR (FOLD / REDUCE)
% Somar, Contar, Máximo, Mínimo.
% Ex: my_sum([1,2,3], S).
my_sum(L, Total) :- my_sum_acc(L, 0, Total).
my_sum_acc([], Acc, Acc).
my_sum_acc([H|T], Acc, Total) :-
    NewAcc is Acc + H,
    my_sum_acc(T, NewAcc, Total).

% ------------------------------------------------------------------------------
% C. OPERAÇÕES DE CONJUNTOS & EXTRA (FERRAMENTAS ÚTEIS)
% ------------------------------------------------------------------------------

% C1. REMOVER DUPLICADOS (Manual do 'setof')
remove_dups([], []).
remove_dups([H|T], [H|R]) :-
    \+ my_member(H, T), !,
    remove_dups(T, R).
remove_dups([_|T], R) :-
    remove_dups(T, R).

% C2. INTERSEÇÃO (Elementos comuns a duas listas)
my_intersection([], _, []).
my_intersection([H|T], L2, [H|R]) :-
    my_member(H, L2), !,
    my_intersection(T, L2, R).
my_intersection([_|T], L2, R) :-
    my_intersection(T, L2, R).

% C3. SUBTRAÇÃO (Elementos de L1 que NÃO estão em L2)
my_subtract([], _, []).
my_subtract([H|T], L2, R) :-
    my_member(H, L2), !,
    my_subtract(T, L2, R).
my_subtract([H|T], L2, [H|R]) :-
    my_subtract(T, L2, R).

% C4. APLANAR (FLATTEN) - [[1,2], [3], [[4]]] -> [1,2,3,4]
% Sai muitas vezes quando usamos findall dentro de findall.
my_flatten([], []).
my_flatten([H|T], Flat) :-
    is_list(H), !,
    my_flatten(H, FH),
    my_flatten(T, FT),
    my_append(FH, FT, Flat).
my_flatten([H|T], [H|Flat]) :-
    my_flatten(T, Flat).

% ------------------------------------------------------------------------------
% D. FINDALL SEM FINDALL
% ------------------------------------------------------------------------------
% Só usar se proibirem explicitamente o findall nativo.
:- dynamic storage_box/1.

manual_findall(Template, Goal, List) :-
    (
        call(Goal),
        assertz(storage_box(Template)),
        fail
    ;
        true
    ),
    collect_manual(List).

collect_manual([H|T]) :-
    retract(storage_box(H)), !,
    collect_manual(T).
collect_manual([]).

% ------------------------------------------------------------------------------
% D. ALGORITMOS DE BUSCA (GRAFOS)
% ------------------------------------------------------------------------------
% ATENÇÃO: No teste tens de definir o predicado edge(A, B) ou connected(A, B).

% DFS (Depth-First Search) - Sem bibliotecas
% Retorna o caminho inverso ou direto dependendo da necessidade
dfs(Start, Goal, Path) :-
    dfs_rec(Start, Goal, [Start], RevPath),
    my_reverse(RevPath, Path).

dfs_rec(Goal, Goal, Visited, Visited).
dfs_rec(Curr, Goal, Visited, Path) :-
    % Substituir 'edge' pelo predicado do enunciado (ex: connected)
    edge(Curr, Next),
    \+ my_member(Next, Visited),
    dfs_rec(Next, Goal, [Next|Visited], Path).

% BFS (Breadth-First Search) - Menor caminho em grafos sem peso
bfs(Start, Goal, Path) :-
    bfs_queue([[Start]], Goal, RevPath),
    my_reverse(RevPath, Path).

bfs_queue([[Goal|Path]|_], Goal, [Goal|Path]).
bfs_queue([CurrentPath|RestQueue], Goal, FinalPath) :-
    CurrentPath = [Node|_],
    findall([Next, Node|CurrentPath],
            (edge(Node, Next), \+ my_member(Next, CurrentPath)),
            NewPaths),
    my_append(RestQueue, NewPaths, UpdatedQueue),
    bfs_queue(UpdatedQueue, Goal, FinalPath).

% ------------------------------------------------------------------------------
% E. HIGHER ORDER (META-PREDICADOS MANUAIS)
% ------------------------------------------------------------------------------

% MY_MAPLIST: Aplica Pred(X, Y) a listas
my_maplist(_, [], []).
my_maplist(Pred, [H1|T1], [H2|T2]) :-
    call(Pred, H1, H2),
    my_maplist(Pred, T1, T2).

% MY_FILTER: (Include) Mantém X se Pred(X) for verdade
my_filter(_, [], []).
my_filter(Pred, [H|T], [H|R]) :-
    call(Pred, H), !,
    my_filter(Pred, T, R).
my_filter(Pred, [_|T], R) :-
    my_filter(Pred, T, R).

% MY_FOLDL: Acumulador (Esquerda para Direita)
% Ex: somar lista -> my_foldl(plus, 0, [1,2,3], Sum).
my_foldl(_, Acc, [], Acc).
my_foldl(Pred, Acc, [H|T], Final) :-
    call(Pred, Acc, H, NextAcc),
    my_foldl(Pred, NextAcc, T, Final).


% ------------------------------------------------------------------------------
% EPOCA ESPECIAL 
% ------------------------------------------------------------------------------

% ==============================================================================
% BASE DE CONHECIMENTO (FACTOS)
% ==============================================================================

% Necessário para a Pergunta 13 (changeSelf) funcionar, pois vamos alterar factos.
:- dynamic by/3.

% by(Character, Movie, Actor)
by(jackRyan, theSumOfAllFears, benAffleck).
by(cathyMuller, theSumOfAllFears, bridgetMoynahan).
by(jackRyan, theHuntForRedOctober, alecBaldwin).
by(jackRyan, patriotGames, harrisonFord).
by(cathyMuller, patriotGames, anneArcher).
by(jackRyan, clearAndPresentDanger, harrisonFord).
by(cathyMuller, clearAndPresentDanger, anneArcher).
by(president, airForceOne, harrisonFord).
by(frasierCrane, cheers, kelseyGrammer).
by(frasierCrane, frasier, kelseyGrammer).
by(rachelGreen, friends, jenniferAniston).
by(monicaGeller, friends, courteneyCox).
by(phoebeBuffay, friends, lisaKudrow).
by(ursulaBuffay, friends, lisaKudrow).
by(joeyTribbiani, friends, mattLeBlanc).
by(joeyTribbiani, joey, mattLeBlanc).
by(alexGarrett, joey, andreaAnders).
by(stephenColbert, dailyShow, stephenColbert).
by(stephenColbert, theColbertReport, stephenColbert).
by(addisonMontgomery, privatePractice, kateWalsh).
by(addisonMontgomery, greysAnatomy, kateWalsh).
by(mattMurdock, daredevil, benAffleck).
by(elektraNatchios, daredevil, jenniferGarner).
by(elektraNatchios, elektra, jenniferGarner).
by(elektraNatchios, elektra, lauraWard).
by(sydneyBristow, alias, jenniferGarner).

% ==============================================================================
% PERGUNTA 11
% ==============================================================================
% Enunciado:
% Implemente o predicado plays_twins(?Actor, ?Movie), que sucede se o ator Actor 
% tiver interpretado pelo menos dois personagens diferentes no filme/série Movie, 
% tipicamente indicativo de ter representado personagens gémeas.
% (SEM findall, setof, bagof ou bibliotecas).

plays_twins(Actor, Movie):-
    by(Char1, Movie, Actor),
    by(Char2, Movie, Actor),
    Char1 \= Char2.

% ==============================================================================
% PERGUNTA 12
% ==============================================================================
% Enunciado:
% Implemente o predicado actor_movies(+Actor, -Movies) que devolve em Movies a 
% lista de filmes/séries distintos protagonizados por Actor (a ordem não é 
% relevante, mas não deve ter repetidos).
% (SEM findall, setof, bagof ou bibliotecas).

actor_movies(Actor, L):-
    actor_movies(Actor, [], L).

% Caso recursivo: Encontra um filme novo, adiciona ao acumulador e corta (!).
actor_movies(Actor, Acc, L):-
    by(_Char, Movie, Actor),
    \+ member(Movie, Acc),
    !, 
    actor_movies(Actor, [Movie|Acc], L).

% Caso base: Não há mais filmes novos, o acumulador é o resultado.
actor_movies(_Actor, Acc, Acc).

% ==============================================================================
% PERGUNTA 13
% ==============================================================================
% Enunciado:
% Implemente o predicado changeSelf/0 que atualiza a base de conhecimento, 
% substituindo factos em que a personagem e o ator são o mesmo para que a 
% personagem seja indicada como self. Note que o predicado deve sempre suceder.

changeSelf:-
    % 1. Encontra facto onde Ator == Personagem e remove-o
    retract( by(Actor, Movie, Actor) ),
    % 2. Insere o novo facto com 'self'
    assert( by(self, Movie, Actor) ),
    % 3. Falha propositadamente para forçar o backtracking e procurar mais casos
    fail.

% 4. Quando não houver mais casos (o retract falhar), entra nesta cláusula e termina com sucesso.
changeSelf.

% ==============================================================================
% PERGUNTA 14
% ==============================================================================
% Enunciado:
% Implemente o predicado playedBy(+Character, -List), que devolve em List uma 
% lista com todos os atores que representaram a personagem Character, contendo 
% uma lista com os respectivos filmes/séries.
% Formato: [ Ator1-[FilmeA, FilmeB], Ator2-[FilmeC] ]

playedBy(Character, List):-
    % 1. Obtém lista de Atores únicos (ignorando o filme com ^)
    setof(Actor, Movie^by(Character, Movie, Actor), Actors),
    
    % 2. Para cada ator, encontra todos os seus filmes
    findall(A-Ms, ( 
        member(A, Actors), 
        findall(M, by(Character, M, A), Ms) 
    ), List).

% ==============================================================================
% PERGUNTA 15
% ==============================================================================
% Enunciado:
% Implemente o predicado most_popular(+Exclude, -List, -NMovies), que devolve 
% em List os nomes dos atores que participaram em mais filmes diferentes 
% (lista em caso de empate) e em NMovies o número de filmes.
% Não considerar atores na lista Exclude.

most_popular(Exclude, List, NMovies):-
    % 1. Cria lista ordenada de N-Actor. O setof ordena por N (ascendente).
    setof(N-Actor, (_Char,_Movie,_Movies)^(  
        by(_Char, _Movie, Actor), 
        \+ member(Actor, Exclude),
        actor_movies(Actor, _Movies), % Usa o predicado da P12
        length(_Movies, N) 
    ), TempL),
    
    % 2. Pega no último elemento (que tem o N mais alto)
    last(TempL, NMovies-_),
    
    % 3. Recupera todos os atores que tenham esse NMovies (para os empates)
    findall(Actor, member(NMovies-Actor, TempL), List).

% (Se o teu Prolog não tiver 'last/2' nativo, descomenta a linha abaixo:)
% last([X], X). last([_|T], X) :- last(T, X).

% ==============================================================================
% PERGUNTA 16
% ==============================================================================
% Enunciado:
% Implemente connection_link(+Actor1, +Actor2, -ConnectionList), que determina 
% uma ligação entre dois atores. Se existir, a lista deve ter nomes e filmes.
% Ex: [Ator1, FilmeX, AtorY, FilmeZ, Ator2].

connection_link(Actor1, Actor2, List):-
    % Inicia a pesquisa com o Actor1 na lista de visitados
    connection_link(Actor1, Actor2, [Actor1], RList),
    reverse(RList, List).

% Caso Base: Ligação direta via um filme comum
connection_link(Actor1, Actor2, Tmp, [Actor2, Movie | Tmp]):-
    by(_C1, Movie, Actor1),
    \+ member(Movie, Tmp), % Evita repetir filmes no caminho
    by(_C2, Movie, Actor2),
    Actor1 \= Actor2.

% Passo Recursivo: Encontra um intermediário (Actor3)
connection_link(Actor1, Actor2, Tmp, List):-
    by(_C1, Movie, Actor1),
    \+ member(Movie, Tmp),
    by(_C2, Movie, Actor3),
    Actor1 \= Actor3,
    \+ member(Actor3, Tmp), % Evita ciclos de atores
    % Chama recursivamente a partir do intermediário
    connection_link(Actor3, Actor2, [Actor3, Movie | Tmp], List).

% ==============================================================================
% PERGUNTA 17
% ==============================================================================
% Enunciado:
% Implemente pretty_print(+ConnectionList), que recebe a lista da questão anterior
% e imprime no formato:
% "X worked in Movie with Y, who worked in Movie2 with Z."

pretty_print([A, B, C|T]):-
    write(A), write(' worked in '), write(B), write(' with '), write(C),
    pretty_print2(T).

% Final da frase
pretty_print2([]):- 
    write('.'), nl.

% Continuação da frase
pretty_print2([B, C|T]):-
    write(','), nl, 
    write(' who worked in '), write(B), write(' with '), write(C),
    pretty_print2(T).