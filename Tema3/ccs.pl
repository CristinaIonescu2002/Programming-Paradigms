:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').

:- use_module(library(clpfd)).


% tile/2
% tile(Index, Tile)
%
% Fiecare soluție a predicatului tile este o corespondență între index
% (numărul piesei în lista din enunț) și reprezentarea internă a piesei
% respective.
%
% Puteți alege orice reprezentare doriți, în așa fel încât să puteți
% reprezenta toate piesele din enunț.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiște.
% Pot exista cel mult 1 drum și cel mult 2 castele pe aceeași piesă.
%
% Reprezentarea trebuie să poată fi rotită (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie să definiți reprezentări pentru fiecare dintre cele 16 piese
% din enunțul temei.
%
% Exemplu: apelul tile(1, T1). trebuie să lege T1 la reprezentarea pe
% care o faceți pentru piesa 1. Această reprezentare poate fi transmisă
% celorlalte predicate din temă, pentru a întreba, de exemplu, ce se
% află pe muchia de nord a piesei 1, sau dacă piesa 1 se potrivește cu o
% altă piesă.
% tile(_, _) :- false.
% campie = 0, drum = 1, cetate1 = 2, cetate2 = 3; w, e, s, w
tile(1, [2, 2, 0, 2]).
tile(2, [2, 2, 1, 2]).
tile(3, [2, 2, 0, 0]).
tile(4, [2, 3, 0, 0]).
tile(4, [2, 2, 0, 0]).
tile(5, [2, 0, 3, 0]).
tile(5, [2, 0, 2, 0]).
tile(6, [2, 0, 2, 0]).
tile(7, [2, 0, 0, 0]).
tile(8, [2, 2, 1, 1]).
tile(9, [2, 0, 1, 1]).
tile(10, [2, 1, 1, 0]).
tile(11, [2, 1, 0, 1]).
tile(12, [2, 1, 1, 1]).
tile(13, [0, 0, 1, 1]).
tile(14, [0, 1, 0, 1]).
tile(15, [0, 1, 1, 1]).
tile(16, [1, 1, 1, 1]).

% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevărat dacă pe piesa Tile are pe muchia de pe
% direcția Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentând cetate,
% drum, sau pajiște.
%
% De exemplu, piesa 4 are cetate în nord și în este, și pajiște în sud
% și vest. Iar piesa 10 are cetate în nord, drum în este și sud, și
% pajiște în vest.
%
% Dacă What nu este legat, trebuie legat la entitatea care se află pe
% muchia din direcția Dir.

at(_, _, _) :- false.

at([0, _, _, _], n, p).
at([1, _, _, _], n, d).
at([2, _, _, _], n, c).

at([_, 0, _, _], e, p).
at([_, 1, _, _], e, d).
at([_, 2, _, _], e, c).
at([_, 3, _, _], e, c).

at([_, _, 0, _], s, p).
at([_, _, 1, _], s, d).
at([_, _, 2, _], s, c).
at([_, _, 3, _], s, c).

at([_, _, _, 0], w, p).
at([_, _, _, 1], w, d).
at([_, _, _, 2], w, c).
at([_, _, _, 3], w, c).

% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevărat dacă piesa Tile are entitatea what pe toate
% direcțiile din lista Directions, cu aceleași valori pentru entități și
% direcții ca și la predicatul at/3.
%
% De exemplu, predicatul este adevărat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], și pentru entitatea c. Este adevărat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], și pentru
% entitatea d.
%
% Atenție! Pentru ca predicatul să fie adevărat, nu este nevoie ca în
% Directions să fie *toate* direcțiile pe care se află entitatea
% respectivă, pot fi doar o submulțime a acestora.
% De exemplu, la piesa 14, predicatul este adevărat pentru entitatea d
% și pentru oricare dintre listele [w], [e] sau [e,w].
atL(_, _, _) :- false.

atL(T, [N, S, E, V], W) :- at(T, N, W), at(T, S, W), at(T, E, W), at(T, V, W).

atL(T, [A, B, C], W) :- at(T, A, W), at(T, B, W), at(T, C, W).
atL(T, [A, B], W) :- at(T, A, W), at(T, B, W).
atL(T, [A], W) :- at(T, A, W).


% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).
hasTwoCitadels(_) :- false.

hasTwoCitadels([2, 3, _, _]).
hasTwoCitadels([2, _, 3, _]).
hasTwoCitadels([2, _, _, 3]).

hasTwoCitadels([_, 2, 3, _]).
hasTwoCitadels([_, 2, _, 3]).

hasTwoCitadels([_, _, 2, 3]).

% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevărat dacă RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotită de Rotation ori, în sens trigonometric.
%
% De exemplu, dacă T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiște la nord și
% vest, și cetate la est și sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotații este
% identică cu piesa inițială.
% De exemplu, la piesele 5, 6 și 14, rotirea cu Rotation=2 va duce la o
% reprezentare identică cu piesa inițială, și la fel rezultatele pentru
% Rotation=1 și Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie să aibă aceeași reprezentare cu
% reprezentarea inițială.
ccw(_, _, _) :- false.

ccw(T, 0, T).
ccw([A, B, C, D], 1, [B, C, D, A]).
ccw([A, B, C, D], 2, [C, D, A, B]).
ccw([A, B, C, D], 3, [D, A, B, C]).

% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagă RotationPairs la o listă de perechi
% (Rotation, RotatedTile)
% în care Rotation este un număr de rotații între 0 și 3 inclusiv și
% RotatedTile este reprezentarea piesei Tile rotită cu numărul respectiv
% de rotații.
%
% Rezultatul trebuie întotdeauna să conțină perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie să conțină rotații duplicate. De exemplu, pentru
% piesele 5,6 și 14 rezultatul va conține doar 2 perechi, iar pentru
% piesa 16 rezultatul va conține o singură pereche.
%
% Folosiți recursivitate (nu meta-predicate).
rotations(_, _) :- false.

%rotations(_, T) :- [T].
%rotations(Tile, [(0, _) | T]) :-
%    \+ member((_, Tile), T),
%    append((0, Tile), T, List),
%    [List].
%rotations(Tile, [(N, _) | T]) :- 
%    ccw(Tile, N, R),
%    \+ member((_, R), T),
%    append((N, R), T, List),
%    N1 is N - 1,
%    rotations(Tile, [(N1, R) | List]).


% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul întoarce adevărat dacă NeighborTile poate fi pusă în
% direcția NeighborDirection față de Tile și se potrivește, adică muchia
% comună este de același fel.
%
% De exemplu, dacă T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevărat.
%
% Similar, pentru piesele 8 și 10, este adevărat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% Puteți folosi predicatul opposite/2 din utils.pl.
match(_, _, _) :- false.

match([A, _, _, _], [_, _, A, _], n).
match([_, A, _, _], [_, _, _, A], e).
match([_, _, A, _], [A, _, _, _], s).
match([_, _, _, A], [_, A, _, _], w).


% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagă Rotation la rotația (între 0 și 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel încât piesa s ă se potrivească cu
% vecinii din Neighbors.
%
% Neighbors este o listă de perechi (NeighborTile, NeighborDirection) și
% specifică că pe direcția NeighborDirection se află piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors s ă conțină mai
% puțin de 4 elemente.
%
% Se vor da toate soluțiile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacă la nord se află piesa 14 rotită o
% dată (drumul este vertical), iar la sud se află piesa 2 rotită de 2
% ori (drumul este spre nord), atunci posibilele rotații pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie s ă aibă 2 soluții, în care
% leagă R la 1, și la 3.
% În același exemplu, dacă am avea și piesa 1 ca vecin spre est, atunci
% soluția de mai sus s -ar reduce doar la rotația 3.
%
% Hint: Prolog face backtracking automat. Folosiți match/3.
findRotation(_, _, _) :- false.

%finrRotation(T, (Nt, Nd))

findRotation(_,[],_).
findRotation(Tile, [(NT, ND) | RN], R) :-
    ccw(Tile, R, RT),
    match(RT, NT, ND),
    findRotation(Tile, RN, R).