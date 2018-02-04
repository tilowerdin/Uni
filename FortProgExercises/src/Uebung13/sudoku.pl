:- use_module(library(clpfd)).

sudoku(Rows) :-

% Alle Zellen dürfen nur die Ziffern 1 bis 9 enthalten. Hierfür nutzen wir
% das Prädikat `append/2`, das erfüllt ist, wenn das erste
% Argumente eine Liste von Listen und das zweite Argument deren
% Konkatenation ist (entspricht `concat` in Haskell)
    append(Rows,Cells)
  , Cells ins 1..9    % SWI-Prolog
  % , domain(Cells,1,9) % Sicstus-Prolog

% Die Zellen jeder Zeile müssen unterschiedlich sein
  , each_distinct(Rows)


% Das Gleiche gilt für die Spalten:
  , trans(Rows,Cols), each_distinct(Cols)


% und für die Quadrate:
  , squaresOf9(Rows,Squares), each_distinct(Squares)

% Zum Schluss das Aufzählen:
  , labeling([],Cells).

writeSudoku([]).
writeSudoku([Z|Zs]) :- write(Z), nl, writeSudoku(Zs).

% Zeilen jeder Zeile unterschiedlich
each_distinct([]).
each_distinct([R|Rows]) :-
  all_different(R),
  each_distinct(Rows).
 
% transformieren
trans(X,X) :-
  empty_lists(X).
trans([R|Rows],[C|Cols]) :-
  mySplit(C,RowTails,[R|Rows]),
  trans(RowTails,Cols).

mySplit([],[],[]).
mySplit([Elem|Elems],[RowTail|RowTails],[[Elem|RowTail]|Rows]) :-
  mySplit(Elems,RowTails,Rows).

empty_lists([]).
empty_lists([[]|Ls]) :- empty_lists(Ls).

% 3x3 quadrate in listen fassen
squaresOf9([],[]).
squaresOf9([F,S,T|Rows],Squares) :- %First, Second, Third
  threes(FF,SF,TF,F), % Firstthree of First, Secondthree of First,...
  threes(FS,SS,TS,S),
  threes(FT,ST,TT,T),

  append(FF,FS,FH), %First square Half ready
  append(FH,FT,FSquare), % First Square

  append(SF,SS,SH),
  append(SH,ST,SSquare),

  append(TF,TS,TH),
  append(TH,TT,TSquare),

  append([FSquare],[SSquare],HSquare), %list of Squares Half ready
  append(HSquare,[TSquare],SomeSquares),

  append(SomeSquares,MoreSquares,Squares),
  
  squaresOf9(Rows,MoreSquares).
  
threes([A,B,C],[D,E,F],[G,H,I],[A,B,C,D,E,F,G,H,I]).

start(Puzzle) :-
  Puzzle =
    [[9,_,_,2,_,_,5,_,_],
     [_,4,_,_,6,_,_,3,_],
     [_,_,3,_,_,_,_,_,6],
     [_,_,_,9,_,_,2,_,_],
     [_,_,_,_,5,_,_,8,_],
     [_,_,7,_,_,4,_,_,3],
     [7,_,_,_,_,_,1,_,_],
     [_,5,_,_,2,_,_,4,_],
     [_,_,1,_,_,6,_,_,9]],
   sudoku(Puzzle),
   writeSudoku(Puzzle).