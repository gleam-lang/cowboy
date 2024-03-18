-module(gleam_cowboy_native).

-export([init/2, start_link/2, read_entire_body/1, set_headers/2]).

start_link(Handler, Port) ->
    RanchOptions = #{
        max_connections => 16384,
        num_acceptors => 100,
        socket_opts => [{port, Port}]
    },
    CowboyOptions = #{
        env => #{dispatch => [{'_', [], [{'_', [], ?MODULE, Handler}]}]},
        stream_handlers => [cowboy_stream_h]
    },
    ranch_listener_sup:start_link(
        {gleam_cowboy, make_ref()},
        ranch_tcp, RanchOptions,
        cowboy_clear, CowboyOptions
    ).

init(Req, Handler) ->
    {ok, Handler(Req), Req}.

read_entire_body(Req) ->
    read_entire_body([], Req).

read_entire_body(Body, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Chunk, Req1} -> {list_to_binary([Body, Chunk]), Req1};
        {more, Chunk, Req1} -> read_entire_body([Body, Chunk], Req1)
    end.

set_headers(Headers, Req) ->
  Req#{resp_headers => Headers}.
