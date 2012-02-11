-module(talk_server).
-export([start/0, server/0]).
-include("talk_interface.hrl").
-include("talk_config.hrl").
-record(user_entry, {name, pid}).

start() ->
    register(messenger, spawn_link(?MODULE, server, [])).

server() ->
    server([]).

server(UserList) ->
    io:format("UserList = ~p~n", [UserList]),
    receive
        #logon{client_pid=Pid, username=Name} ->
            UserList2 = logon(UserList, Pid, Name),
            server(UserList2);
        #message{from_pid=FromPid, to_name=ToName, message=Msg} ->
            transfer(FromPid, ToName, Msg, UserList),
            server(UserList)
    end.

logon(UserList, Pid, Name) ->
    case lists:keymember(Name, #user_entry.name, UserList) of
        false ->
            Pid ! #server_reply{message=welcome},
            [#user_entry{pid=Pid, name=Name} | UserList];
        true ->
            Pid ! #abort_client{message=user_exists_at_other_node},
            UserList
    end.

transfer(FromPid, ToName, Msg, UserList) ->
    case lists:keyfind(FromPid, #user_entry.pid, UserList) of
        #user_entry{name=FromName} ->
            transfer(FromPid, FromName, ToName, Msg, UserList);
        false ->
            FromPid ! #abort_client{message=not_logged_on}
    end.

transfer(FromPid, FromName, ToName, Msg, UserList) ->
    case lists:keyfind(ToName, #user_entry.name, UserList) of
        #user_entry{pid=ToPid} ->
            ToPid ! #message_from{from=FromName, message=Msg},
            FromPid ! #server_reply{message=sent};
        false ->
            FromPid ! #server_reply{message=receiver_not_found}
    end.
