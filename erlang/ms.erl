-module(ms).
-export([start/1, to_slave/2, quit/0, master_init/1, slave/1]).

start(N) ->
    register(master, spawn_link(ms, master_init, [N])).

to_slave(Msg, N) ->
    master ! { msg_to_slave, Msg, N }.

quit() ->
    master ! quit.

master_init(N) ->
    process_flag(trap_exit, true),
    Slaves = master_init(N, []),
    master_run(Slaves).

master_init(0, Slaves) -> Slaves;
master_init(N, Slaves) ->
    master_init(N-1, create_slave(N, Slaves)).

create_slave(N, Slaves) ->
    Pid = spawn_link(ms, slave, [N]),
    [{Pid, N} | Slaves].
    
slave(N) ->
    receive
        die -> exit(die);
        Msg ->
            io:format("Slave N got message ~n"),
            slave(N)
    end.

master_run(Slaves) ->
    receive
        {msg_to_slave, Msg, N} ->
            Slave = find_slave(Slaves, N),
            Slave ! Msg,
            master_run(Slaves);
        {'EXIT', Slave, _} ->
            io:format("master restarting dead slave ~n"),
            Slaves2 = recreate_slave(Slaves, Slave),
            master_run(Slaves2);
        quit ->
            lists:foreach(fun({Slave, _}) -> Slave ! die end, Slaves)
    end.
    
find_slave([], _) -> not_found;
find_slave([{Pid, Id} | _], Id) ->
    Pid;
find_slave([_|T], Id) ->
    find_slave(T, Id).

recreate_slave(Slaves, Pid) ->
    recreate_slave(Slaves, Pid, []).

recreate_slave([], _, NewSlaves) ->
    NewSlaves; % not found;
recreate_slave([{Pid, Id} | T], Pid, NewSlaves) -> 
    lists:append(create_slave(Id, NewSlaves), T);
recreate_slave([H|T], Pid, NewSlaves) -> 
    recreate_slave(T, Pid, [H|NewSlaves]).
