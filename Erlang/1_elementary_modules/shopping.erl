-module(shopping).
-export([total/1, total2/1, double_shopping/1]).

total([]) -> 0;
total(L) ->
    [LHead | LTail] = L,
    total = element(1, LHead) + total(LTail).

total2(L) ->
    [Value || {_, Value} <- L].

double_shopping(L) ->
    [2 * Value || {_, Value} <- L].
