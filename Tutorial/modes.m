
% Modes

:- module modes.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module list, string, int, float, math.

% Combined declarations
% :- pred phone(string::in, int::out) is semidet.

% Separate declarations
:- pred phone(string, int).
:- mode phone(in, out) is semidet.
:- mode phone(out, in) is nondet.

phone("Ian", 66532).
phone("Julien", 66532).
phone("Peter", 66540).
phone("Ralph", 66532).
phone("Zoltan", 66514).

:- pred square(int::in, int::out) is det.

square(X, X * X).

:- pred absolute_square_root(float::in, float::out) is semidet.

absolute_square_root(X, AbsSqrtX) :-
  X >= 0.0, AbsSqrtX = math.sqrt(X).

:- pred small_prime(int::out) is multi.

small_prime(2).
small_prime(3).
small_prime(5).
small_prime(7).

:- pred small_prime_factor(int::in, int::out) is nondet.

small_prime_factor(X, P) :-
  small_prime(P), X mod P = 0.

:- pred has_small_prime_factor(int::in) is semidet.

has_small_prime_factor(X) :-
  small_prime(P), X mod P = 0.

:- type ott ---> one ; two ; three.

% Exhaustive switch
:- pred p(ott::in, int::out) is det.

p(X, Y) :- (X = one, Y = 1 ; X = two, Y = 2 ; X = three, Y = 3).

% Inexhaustive switch
:- pred q(ott::in, int::out) is semidet.

q(X, Y) :- (X = one, Y = 1 ; X = three, Y = 3).

% Declare insts
:- inst mnon_empty_list == bound([ground | ground]).
:- inst even_length_list == bound([] ; [ground | odd_length_list]).
:- inst odd_length_list == bound([ground | even_length_list]).

:- pred head(list(T), T).
:- mode head(in, out) is semidet.
:- mode head(in(mnon_empty_list), out) is det.

head(Xs, X) :- Xs = [X | _].

main(!IO) :-

  % Semideterministic
  "Ralph" = NameR,
  io.format("%s: %d\n",
  [s(NameR), i(if phone(NameR, Number) then Number else 0)], !IO),

  % Nondeterministic
  66532 = Num,
  io.format("%s: %d\n",
  [s(if phone(NameX, Num) then NameX else "---"), i(Num)], !IO),

  % Square number
  square(5, Sq), io.format("Square: %d\n",[i(Sq)], !IO),

  % Absolute square root
  io.format("Abs: %f\n",
  [f(if absolute_square_root(-16.0, AbSqrt) then AbSqrt else 0.0)], !IO),

  % Prime larger than 5
  io.format("Prime: %d\n",
  [i((if small_prime(X), X > 5 then X else 0))], !IO),

  % Small prime factor of 15 larger than 3
  io.format("Factor: %d\n",
  [i((if small_prime_factor(15, Spf), Spf > 3 then Spf else 0))], !IO),

  % At most 1 answer
  io.format("Has small: %s\n",
  [s((if has_small_prime_factor(10) then "Yes" else "No"))], !IO),

  % Deterministic success and nondeterministic failure
  p(one, Ex), io.format("Ex: %d\n", [i(Ex)], !IO),
  io.format("Inex: %d\n", [i((if q(two, Inex) then Inex else 0))], !IO),

  % Negation
  small_prime(A),
  io.write_string((if not A = 7 then "7 \\= A" else "7 = A"), !IO),
  io.nl(!IO),

  % Alternative 'if else' syntax
  io.format("Alt: %d\n", [i(2 = A -> 22 ; 10)], !IO),

  % List is not empty (deterministic)
  head([5,6,7], Fhd), io.format("FHead: %d\n", [i(Fhd)], !IO),

  % Empty list (semideterministic)
  io.format("EHead: %d\n", [i(if head([], Ehd) then Ehd else 1)], !IO).

