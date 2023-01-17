
% Hello, World!

% Specify name of the module (same as the file name)
:- module hello.

% Export portion of the module
:- interface.

% Import names exported by 'io' module
:- import_module io.

% Define deterministic 'main' predicate with two arguments
:- pred main(io::di, io::uo) is det.

% Private implementation
:- implementation.

:- pred ver1(io::di, io::uo) is det.

ver1(IOState_in, IOState_out) :-
  io.write_string("Hello, World!\n", IOState_in, IOState_out).

:- pred ver2(io::di, io::uo) is det.

ver2(IOState_in, IOState_out) :-
  io.write_string("Hello, ", IOState_in, IOState_1),
  io.write_string("World!", IOState_1, IOState_2),
  io.nl(IOState_2, IOState_out).

:- pred ver3(io::di, io::uo) is det.

ver3(!IO) :-
  io.write_string("Hello", !IO),
  io.write_string(", ", !IO),
  io.write_string("World!", !IO),
  io.nl(!IO).

main(!IO) :-
  ver1(!IO), ver2(!IO), ver3(!IO).

