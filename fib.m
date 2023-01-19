
% Fibonacci Numbers

:- module fib.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

% Import modules for operations on integers, lists and strings
:- import_module int, list, string.

% Predicate version
:- pred pver(io::di, io::uo) is det.

pver(!IO) :- fib(16, X),
  io.write_string("fib(16, ", !IO),
  io.write_int(X, !IO),
  io.write_string(")\n", !IO).

% Predicate declaration and definition
:- pred fib(int::in, int::out) is det.

fib(N, X) :-
  (  if    N =< 2
     then  X = 1
     else  fib(N - 1, A), fib(N - 2, B), X = A + B
  ).

% Function version
:- pred fver(io::di, io::uo) is det.

fver(!IO) :-
  io.write_string("fib(17) = ", !IO),
  io.write_int(fib(17), !IO),
  io.nl(!IO).

% Function declaration and definition
:- func fib(int) = int.

fib(N) = X :-
  (  if    N =< 2
     then  X = 1
     else  X = fib(N - 1) + fib(N - 2)
  ).

% Main loop
:- pred mloop(io::di, io::uo) is det.

mloop(!IO) :-

  % Read line
  io.read_line_as_string(Result, !IO),

  % Test input using switch
  (
     % End of file
     Result = eof,

     % Exit program
     io.format("Bye bye...\n", [], !IO)
  ;
     % Unify
     Result = ok(String),

     (
        % Remove whitespaces and convert
        if    string.to_int(string.strip(String), N)

        % Display value
        then  io.format("fib(%d) = %d\n", [i(N), i(fib_2(N))], !IO)

        % Wrong input
        else  io.format("That isn't a number.\n", [], !IO)
     ),

     % Continue
     mloop(!IO)

  ;
     % Error message
     Result = error(ErrorCode),
     io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

% Function with body in clause head
:- func fib_2(int) = int.

fib_2(N) = (if N =< 2 then 1 else fib_2(N - 1) + fib_2(N - 2)).

% Main predicate
main(!IO) :-
  pver(!IO), fver(!IO), mloop(!IO).

