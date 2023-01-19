
% Rot 13

:- module rot13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module char, list, string.

:- func rot13(char) = char.

rot13(Char) =
  (  if Char = 'a' then 'n'
     else if Char = 'b' then 'o'
     else if Char = 'c' then 'p'
     else if Char = 'd' then 'q'
     else if Char = 'e' then 'r'
     else Char
  ).

:- pred mloop(io::di, io::uo) is det.

mloop(!IO) :-

  % Read character
  io.read_char(Result, !IO),

  (
     % Show rotated character
     Result = ok(Char),
     io.write_char(rot13_2(Char), !IO),

     % Next character
     mloop(!IO)
  ;
     % Exit
     Result = eof
  ;
     % Error
     Result = error(ErrorCode),
     io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

:- func rot13_2(char) = char.

rot13_2(CharIn) = (if prot(CharIn, CharOut) then CharOut else CharIn).

:- pred prot(char::in, char::out) is semidet.

prot('a','n').
prot('b','o').
prot('c','p').
prot('d','q').
prot('e','r').

main(!IO) :-
  io.format("%s %c\n", [s("A:"), c(rot13('a'))], !IO),
  mloop(!IO).

