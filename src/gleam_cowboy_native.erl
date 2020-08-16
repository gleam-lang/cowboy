-module(gleam_cowboy_native).

-export([init/2, start_link/2]).

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
