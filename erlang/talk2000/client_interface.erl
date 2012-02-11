-module(client_interface).
-export([logon/1, message/2]).
-include("talk_interface.hrl").
-include("talk_config.hrl").

logon(Name) ->
    register(client, spawn(talk_client, client, [?server_node, Name])).

message(ToName, Msg) ->
    client ! #message_to{to_name=ToName, message=Msg}.
