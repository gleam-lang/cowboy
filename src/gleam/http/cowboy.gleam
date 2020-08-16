import gleam/atom.{Atom}
import gleam/map.{Map}
import gleam/http
import gleam/otp/process.{Pid}
import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{Dynamic}

external type CowboyRequest

external fn erlang_start_link(
  handler: fn(CowboyRequest) -> CowboyRequest,
  port: Int,
) -> Result(Pid, Dynamic) =
  "gleam_cowboy_native" "start_link"

external fn cowboy_reply(
  Int,
  Map(String, String),
  BitBuilder,
  CowboyRequest,
) -> CowboyRequest =
  "cowboy_req" "reply"

fn service_to_handler(
  service: http.Service(BitString, BitBuilder),
) -> fn(CowboyRequest) -> CowboyRequest {
  fn(request) {
    let status = 200
    let headers = map.new()
    let body = bit_builder.from_bit_string(<<"Hello, world":utf8>>)
    cowboy_reply(status, headers, body, request)
  }
}

// TODO: refine error type
// TODO: document
// TODO: test
pub fn start(
  service: http.Service(BitString, BitBuilder),
  on_port number: Int,
) -> Result(Pid, Dynamic) {
  service
  |> service_to_handler
  |> erlang_start_link(number)
}
