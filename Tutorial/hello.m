
% Hello, World!

% Specify name of the module (the same as the file name)
% Export portion of the module
% Import names exported by 'io' module
% Define deterministic 'main' predicate with two arguments
% Begin private implementation
:- module hello.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

% Version 1
:- pred ver1(io::di, io::uo) is det.
ver1(IOState_in, IOState_out) :-
  io.write_string("Hello, World 1!\n", IOState_in, IOState_out).

% Version 2
:- pred ver2(io::di, io::uo) is det.
ver2(IOState_in, IOState_out) :-
  write_string("Hello, ", IOState_in, IOState_1),
  write_string("World 2!", IOState_1, IOState_2),
  nl(IOState_2, IOState_out).

% Version 3
:- pred ver3(io::di, io::uo) is det.
ver3(!IO) :-
  write_string("Hello", !IO),
  write_string(", ", !IO),
  write_string("World 3!", !IO),
  nl(!IO).

% Main predicate
main(!IO) :-
  ver1(!IO), ver2(!IO), ver3(!IO).

