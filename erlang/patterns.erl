-module(patterns).
-export([average/1, double/1, member/2, average2/1, factorial/1]).

%%%%%%%%%%%%%%%%%%%%%%%
average(X) -> sum(X) / len(X).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

len([_|T]) -> 1 + len(T);
len([]) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%
double([H|T]) -> [2*H|double(T)];
% should not be "double([]) -> 0."
double([]) -> [].

member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, []) -> false.


%%%%%%%%%%%%%%%%%%%%%%%
% Only traverses the list ONCE
% Executes in constant space (tail recursive)
% The variables Length and Sum play the role of accumulators
% N.B. average([]) is not defined - (you cannot have the average of zero elements) - evaluating average([]) would cause a run-time error
average2(X) -> average2(X, 0, 0).

average2([H|T], Length, Sum) -> average2(T, Length+1, Sum+H);
average2([], Length, Sum) -> Sum / Length.


%%%%%%%%%%%%%%%%%%%%%%%
factorial(X) when X < 0 -> exit('bad argument');
factorial(X) -> factorial(X, 1).
% same as
%factorial(X) ->
%    if
%        X < 0 -> exit('bad argument, X < 0');
%        true -> factorial(X, 1)
%    end.

factorial(0, Result) -> Result;
factorial(X, Result) -> factorial(X-1, X * Result).
