
% Rot 13

:- module rot13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module char, list, string.

% Rotation predicate
:- pred prot(char::in, char::out) is semidet.
prot('a','n'). prot('b','o').
prot('c','p'). prot('d','q'). prot('e','r').

% Rotation function that uses predicate
:- func rot13(char) = char.
rot13(CharIn) = (if prot(CharIn, CharOut) then CharOut else CharIn).

% Main loop
:- pred mloop(io::di, io::uo) is det.
mloop(!IO) :-

  % Read character
  io.read_char(Result, !IO),

  (
     % Show rotated character
     Result = ok(Char),
     io.write_char(rot13(Char), !IO),

     % Next character
     mloop(!IO)
  ;
     % Exit
     Result = eof,
     write_string("Bye...\n", !IO)
  ;
     % Error
     Result = error(ErrorCode),
     io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

% Standalone function
:- func rot13_2(char) = char.
rot13_2(Char) =
  (
     if       Char = 'a' then 'n'
     else if  Char = 'b' then 'o'
     else if  Char = 'c' then 'p'
     else if  Char = 'd' then 'q'
     else if  Char = 'e' then 'r'
     else     Char
  ).

% Main predicate
% Display rotation using function
% Enter main loop of the program
main(!IO) :-
  io.format("%s %c\n",
    [s("A:"), c(rot13_2('a'))], !IO),
  mloop(!IO).

