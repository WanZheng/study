-module(exercise).
-export([test/0, temp_convert/1, pong/0, circle_pong/1]).

test() ->
    {
        temp_convert({c, 100}), temp_convert({f, 32}),
        perimeter({square, 3}), perimeter({circle, 3}), perimeter({triangle, {{1, 1}, {5, 1}, {5, 4}}}),
        mymin([1, 3, 2]), mymax([1, 3, 2]),
        swedish_date(),
        start_ping(5, "ping"),
        start_circle_ping(5, 3, "circle_ping"),
        start_star_ping(5, 3, "star_ping")
    }.

%%
temp_convert({c, C}) -> {f, 9*C/5 + 32};
temp_convert({f, F}) -> {c, 5*(F-32)/9}.

%%
perimeter({square, Side}) -> 4 * Side;
perimeter({circle, Radius}) -> 2 * 3.14 * Radius;
perimeter({triangle, {A, B, C}}) -> distance(A, B) + distance(A, C) + distance(B, C).

distance({X1, Y1}, {X2, Y2}) -> math:sqrt(square(X1-X2) + square(Y1-Y2)).
square(X) -> X*X.

%%
mymin([H|T]) -> mymin(T, H).

mymin([], Min) -> Min;
mymin([H|T], Min) -> 
    if
        H < Min -> mymin(T, H);
        true -> mymin(T, Min)
    end.

mymax([H|T]) -> mymax(T, H).

mymax([], Max) -> Max;
mymax([H|T], Max) -> 
    if
        H > Max -> mymax(T, H);
        true -> mymax(T, Max)
    end.

%%
swedish_date() ->
    { Y, M, D } = date(),
    [_,_|YY] = integer_to_list(Y),
    MM = a_2_aa(M),
    DD = a_2_aa(D),
    lists:append([YY, MM, DD]).

a_2_aa(A) ->
    if
        A > 10 -> AA = integer_to_list(A);
        true    -> AA = [$0|integer_to_list(A)]
    end,
    AA.

%% Write a function which starts 2 processes, and sends a message M times forewards and backwards between them. After the messages have been sent the processes should terminate gracefully.
start_ping(N, Msg) -> 
    Pong = spawn(exercise, pong, []),
    ping_n(N, Msg, Pong).

ping_n(0, _, Pong) ->
    Pong ! {self(), finished},
    receive
        finished -> io:format("ping finished~n")
    end;
ping_n(N, Msg, Pong) ->
    Pong ! {self(), Msg},
    receive
        Msg -> io:format("receive pong ~p~n", [Msg])
    end,
    ping_n(N-1, Msg, Pong).

pong() ->
    receive
        {From, finished} ->
            io:format("pong finished~n"),
            From ! finished;
        {From, Msg} ->
            io:format("receive ping ~p~n", [Msg]),
            From ! Msg,
            pong()
    end.

%% Write a function which starts N processes in a ring, and sends a message M times around all the processes in the ring. After the messages have been sent the processes should terminate gracefully.
start_circle_ping(NProcess, NTimes, Msg) ->
    Next = spawn_link(exercise, circle_pong, [NProcess-1]),
    circle_ping(Next, NTimes, Msg).

circle_ping(Next, 0, _) ->
    Next ! {self(), finished},
    receive
        {_, finished} -> io:format("ping finished~n")
    end;
circle_ping(Next, NTimes, Msg) ->
    Next ! {self(), Msg},
    receive
        {_, Reply} when Msg == Reply -> io:format("receive pong ~n")
    end,
    circle_ping(Next, NTimes-1, Msg).

circle_pong(0) ->
    receive
        {From, finished} ->
            io:format("last one received finished~n"),
            From ! {From, finished};
        {From, Msg} ->
            io:format("last one received ping~n"),
            From ! {From, Msg},
            circle_pong(0)
    end;
circle_pong(N) ->
    Next = spawn_link(exercise, circle_pong, [N-1]),
    receive
        {From, finished} ->
            io:format("received finished~n"),
            Next ! {From, finished};
        {From, Msg} ->
            io:format("received ping~n"),
            Next ! {From, Msg},
            circle_pong(N)
    end.

%% Write a function which starts N processes in a star, and sends a message to each of them M times. After the messages have been sent the processes should terminate gracefully.
start_star_ping(N, M, Msg) ->
    Children = create_children(N),
    star_ping(Children, M, Msg).

create_children(N) -> create_children(N, []).

create_children(0, Children) -> Children;
create_children(N, Children) ->
    Child = spawn_link(exercise, pong, []),
    create_children(N-1, [Child | Children]).

star_ping(Children, 0, _) ->
    ping_all(Children, finished),
    receive_all(Children), 
    io:format("star ping finished~n");
star_ping(Children, M, Msg) ->
    ping_all(Children, Msg),
    receive_all(Children),
    star_ping(Children, M-1, Msg).

ping_all(Children, Msg) -> 
    Fun = fun(Child) -> Child ! {self(), Msg} end,
    lists:foreach(Fun, Children).

receive_all(Children) ->
    Fun = fun(_) ->
            receive
            _ -> io:format("receive reply ~n")
            end
        end,
    lists:foreach(Fun, Children).
