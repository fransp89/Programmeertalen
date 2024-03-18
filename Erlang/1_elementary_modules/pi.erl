-module (pi).
-export([pi/0, calc/3, calc/2]).

% ============================ %
%  Calls the main pi function. %
% ============================ %

pi() -> calc(1,3,-1).

% =================================================== %
% A - current value in brackets (1 - 1/3 + 1/5 - ...)
% B - denominator
% C - sign(+/-)
%
% Recursive function that calculates pi.
% =================================================== %
calc(A, B) when B >= 15 ->
  A * 4.
calc(A, B, C) ->
  calc(A + 1 / B * C, B + 2, -1 * C).