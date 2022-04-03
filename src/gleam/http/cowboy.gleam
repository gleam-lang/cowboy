import gleam/dynamic.{Dynamic}
import gleam/bit_builder.{BitBuilder}
import gleam/http/cowboy/common.{CowboyRequest, service_to_handler}
import gleam/http/service.{Service}
import gleam/otp/process.{Pid}
import gleam/otp/actor.{StartResult}

external fn erlang_start_link(
  handler: fn(CowboyRequest) -> CowboyRequest,
  port: Int,
) -> Result(Pid, Dynamic) =
  "gleam_cowboy_native" "start_link"

// TODO: document
// TODO: test
pub fn start(
  service: Service(BitString, BitBuilder),
  on_port number: Int,
) -> StartResult(a) {
  service
  |> service_to_handler
  |> erlang_start_link(number)
  |> actor.from_erlang_start_result
}
