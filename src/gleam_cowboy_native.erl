-module(gleam_cowboy_native).

-export([init/2, start_link/2, read_entire_body/1]).

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

% Callback used by websocket.gleam
% Allows for both normal request/response as well as upgrades to a persistant
% websocket connection
init(Req, #{handler => Handler} = Handlers) ->
    case Handler(Req) of
        {cowboy_websocket, Res, State} -> {cowboy_websocket, Res, Handlers#{state => State}, #{max_frame_size => 8000000, idle_timeout => 30000}};
        Res -> {ok, Res, Req}
    end.

% Normal Callback used by cowboy.gleam
init(Req, Handler) ->
    {ok, Handler(Req), Req}.

% https://ninenines.eu/docs/en/cowboy/2.9/guide/ws_handlers/
websocket_init(#{state => State, on_ws_init => OnWSInit } = Handlers) ->
  case OnWSInit(State) of
      {ok, State} -> {ok, Handlers#{state => State}, hibernate};
      {reply, Frame, State} -> {reply, Frame, Handlers#{state => State}, hibernate}
  end.

websocket_handle(Frame, #{state => State, on_ws_frame => OnWSFrame } = Handlers) ->
  case OnWSFrame(State) of
      {ok, State} -> {ok, Handlers#{state => State}, hibernate};
      {reply, Frame, State} -> {reply, Frame, Handlers#{state => State}, hibernate}
  end.

websocket_info(Frame, #{state => State, on_info => OnInfo } = Handlers) ->
  case OnInfo(State) of
      {ok, State} -> {ok, Handlers#{state => State}, hibernate};
      {reply, Frame, State} -> {reply, Frame, Handlers#{state => State}, hibernate}
  end.

read_entire_body(Req) ->
    read_entire_body([], Req).

read_entire_body(Body, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Chunk, Req1} -> {list_to_binary([Body, Chunk]), Req1};
        {more, Chunk, Req1} -> read_entire_body([Body, Chunk], Req1)
    end.
