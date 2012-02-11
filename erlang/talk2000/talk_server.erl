-module(talk_server).
-export([start/0]).

start() ->
    register(server, spawn_link(?MODULE, server, [])).

server() ->
    
