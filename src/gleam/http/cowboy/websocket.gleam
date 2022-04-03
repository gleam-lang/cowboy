import gleam/dynamic.{Dynamic}
import gleam/map.{Map}
import gleam/bit_builder.{BitBuilder}
import gleam/http/cowboy/common.{WSService, ws_service_to_handler}
import gleam/otp/process.{Pid}
import gleam/erlang/atom.{Atom}
import gleam/otp/actor.{StartResult}

external fn erlang_start_ws_link(
  handlers: Map(Atom, Dynamic),
  port: Int,
) -> Result(Pid, Dynamic) =
  "gleam_cowboy_native" "start_link"

pub type Frame {
  Text(String)
}

pub type FrameResponse {
  Ignore
  Respond(Frame)
}

type Next(state) =
  #(FrameResponse, state)

pub fn start(
  service: WSService(BitString, BitBuilder, state),
  on_ws_init init: fn(state) -> Next(state),
  on_ws_frame frame: fn(state, Frame) -> Next(state),
  on_info info: fn(state, Dynamic) -> Next(state),
  on_port number: Int,
) -> StartResult(a) {
  service
  |> ws_service_to_handler
  |> fn(handler) {
    map.from_list([
      #(atom.create_from_string("handler"), dynamic.from(handler)),
      #(atom.create_from_string("on_ws_init"), dynamic.from(init)),
      #(atom.create_from_string("on_ws_frame"), dynamic.from(frame)),
      #(atom.create_from_string("on_info"), dynamic.from(info)),
    ])
  }
  |> erlang_start_ws_link(number)
  |> actor.from_erlang_start_result
}
