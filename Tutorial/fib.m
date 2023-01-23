
% Fibonacci Numbers

% Import modules for operations on integers, lists and strings
:- module fib.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, list, string.

% Predicate declaration and definition
:- pred fib(int::in, int::out) is det.
fib(N, X) :-
  if    N =< 2
  then  X = 1
  else  fib(N - 1, A),
        fib(N - 2, B),
        X = A + B.

% Predicate version
:- pred pver(io::di, io::uo) is det.
pver(!IO) :- fib(16, X),
  write_string("fib(16, ", !IO),
  write_int(X, !IO),
  write_string(")\n", !IO).

% Function declaration and definition
:- func fib(int) = int.
fib(N) = X :-
  if    N =< 2
  then  X = 1
  else  X = fib(N - 1) + fib(N - 2).

% Function version
:- pred fver(io::di, io::uo) is det.
fver(!IO) :-
  write_string("fib(17) = ", !IO),
  write_int(fib(17), !IO),
  nl(!IO).

% Function with body in clause head
:- func fib_2(int) = int.
fib_2(N) = (if N =< 2 then 1 else fib_2(N - 1) + fib_2(N - 2)).

% Main loop
:- pred mloop(io::di, io::uo) is det.
mloop(!IO) :-

  % Read line
  io.read_line_as_string(Result, !IO),

  % Test input using disjunction
  (
     % Exit the program if eof
     Result = eof,
     format("Bye bye...\n", [], !IO)
  ;
     % Unify
     Result = ok(String),

     (
        % Remove whitespaces and convert to int
        % Display value or handle erroneous input
        if    string.to_int(string.strip(String), N)
        then  format("fib(%d) = %d\n", [i(N), i(fib_2(N))], !IO)
        else  format("That isn't a number.\n", [], !IO)
     ),

     % Continue
     mloop(!IO)

  ;
     % Error message
     Result = error(ErrorCode),
     format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

% Main predicate
main(!IO) :-
  pver(!IO), fver(!IO), mloop(!IO).

