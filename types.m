
% Types

:- module types.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, string, int, univ.

% Discriminated unions
:- type playing_card ---> card(rank, suit) ; joker.
:- type rank ---> ace  ; two ; three ; four  ;
                  five ; six ; seven ; eight ;
                  nine ; ten ; jack  ; queen ; king.
:- type suit ---> clubs ; diamonds ; hearts ; spades.
:- type bank_account ---> account(name::string, account_no::int, funds::float).
:- type employee ---> employee(id::int, contact::contact_details).
:- type contact_details ---> contact_details(address::string, phone::int).

% Polymorphic type
:- type tree(T) ---> leaf ; branch(tree(T), T, tree(T)).

% Equivalence types
:- type height == float.
:- type radius == float.
:- type volume == float.
:- type dictionary(Key, Value) == list({Key, Value}).

% Find value in dictionary
:- pred search(dictionary(Key, Value)::in, Key::in, Value::out) is semidet.

search([{K, V} | Dict], Key, Value) :-
 (if Key = K then Value = V else search(Dict, Key, Value)).

% Add value to dictionary
:- func set(dictionary(Key, Value), Key, Value) = dictionary(Key, Value).

set(Dict, Key, Value) = [{Key, Value} | Dict].

% Tree search
:- pred search(tree(T)::in, T::in) is semidet.

search(branch(L, X, R), Y) :-

  % Compare values
  O = ordering(X, Y),

  (
     % Right
     O = (<), search(R, Y)
  ;
     % Success
     O = (=)
  ;
     % Left
     O = (>), search(L, Y)
  ).

% Function as parameter
:- func mmap(func(T1) = T2, list(T1)) = list(T2).

mmap(_, []) = [].
mmap(F, [X | Xs]) = [F(X) | map(F, Xs)].

% Add one
:- func add1(int) = int.

add1(N) = ( 1 + N ).

% Show list of integers
:- pred showInts(list(int)::in, io::di, io::uo) is det.

showInts([], !IO) :- io.nl(!IO).
showInts([H | T], !IO) :-
  io.format("%d ", [i(H)], !IO), showInts(T, !IO).

% Filter list using predicate
% Elements in Ys satisfy condition and those in Zs don't.
% Mode is declared separately
:- pred mfilter(pred(T), list(T), list(T), list(T)).
:- mode mfilter(in(pred(in) is semidet), in, out, out) is det.

% Combined declarations
% :- pred mfilter(pred(T)::in(pred(in) is semidet), list(T)::in, list(T)::out, list(T)::out) is det.

mfilter(_, [], [], []).
mfilter(P, [X | Xs], Ys, Zs) :-

  % Filter rest of list
  mfilter(P, Xs, Ys0, Zs0),

  % Add elements
  (  if P(X)
     then Ys = [X | Ys0], Zs = Zs0
     else Ys = Ys0, Zs = [X | Zs0]
  ).

% Greater than four
:- pred gt4(int::in) is semidet.

gt4(N) :- N > 4.

% Print universal type
:- pred print_univ(univ::in, io::di, io::uo) is det.

print_univ(U, !IO) :-
  (
     % Convert univ to type
     if univ_to_type(U, C) then
       io.format("a char, %c\n", [c(C)], !IO)
     else if univ_to_type(U, S) then
       io.format("a string, \''%s\''\n", [s(S)], !IO)
     else if univ_to_type(U, I) then
       io.format("an int, %d\n", [i(I)], !IO)
     else if univ_to_type(U, F) then
       io.format("a float, %f\n", [f(F)], !IO)
     else
       io.format("no idea...\n", [], !IO)
  ).

main(!IO) :-

  % Second value from tuple
  Tuple = {5, 6},
  {_, Value} = Tuple,

  % First value from list
  List = [2, 5, 8],
  [X | _] = List,

  % Data constructor
  BankAccount = account("Name", 123, 20.15),

  % Set new name
  BankAccount^name := "AccName" = NewAccount,

  % Employee with contact details (addres and phone)
  employee(250, ConDet) = Emp,
  ConDet = contact_details("Address", 222),

  % Tree type
  TreeInt = branch(leaf, 1, branch(leaf, 2, leaf)),
  branch(_, TreeVal, _) = TreeInt,

  % Search binary tree
  BinTree = branch(branch(branch(leaf, 1, leaf), 2, branch(leaf, 3, leaf)), 4,
                   branch(branch(leaf, 5, leaf), 6, branch(leaf, 7, leaf))),
  write_string((if search(BinTree, 5) then "Five\n" else "Not found\n"), !IO),

  % Create dictionary and add new entry
  Dict = [{2, "two"}, {3, "three"}], NewDict = set(Dict, 1, "one"),

  % Map
  IntList = [2, 4, 7, 9], mmap(add1, IntList) = NewList,

  % Filter
  mfilter(gt4, IntList, Sat, NSat),

  % Format string
  io.format("Tuple: %d\nList: %d\nUnion: %d\n\
Update: %s\nPhone: %d\nTree: %d\n\
Dictionary: %s\n",
    [i(Value), i(X), i(BankAccount^account_no),
     s(NewAccount^name), i(Emp^contact^phone), i(TreeVal),
     s(if search(NewDict, 2, DictVal) then DictVal else "No key")], !IO),

  % List after incrementing all values by one
  io.write_string("New List: ", !IO), showInts(NewList, !IO),

  % Filtered list
  io.write_string("Sat: ", !IO), showInts(Sat, !IO),
  io.write_string("NSat: ", !IO), showInts(NSat, !IO),

  % Print universal types
  print_univ(univ(1), !IO),
  print_univ(univ("2"), !IO),
  print_univ(univ(3.4), !IO),
  print_univ(univ({5, 6, 7}), !IO).

