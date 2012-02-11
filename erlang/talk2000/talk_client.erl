-module(talk_client).
-export([client/2]).
-include("talk_interface.hrl").

client(ServerNode, Name) ->
    {messenger, ServerNode} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(ServerNode).

client(ServerNode) ->
    receive
        #message_from{from=FromName, message=Msg} ->
            io:format("~p said ~p~n", [FromName, Msg]);
        #message_to{to_name=ToName, message=Msg} ->
            {messenger, ServerNode} ! #message{from_pid=self(), to_name=ToName, message=Msg},
            await_result()
    end,
    client(ServerNode).

await_result() ->
    receive
        #abort_client{message=Msg} ->
            io:format("abort ~p~n", [Msg]),
            exit(normal);
        #server_reply{message=Msg} ->
            io:format("server reply ~p~n", [Msg])
    end.

        
    
