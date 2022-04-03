import gleam/list
import gleam/pair
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/result
import gleam/http.{Header}
import gleam/http/service.{Service}
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{Dynamic}
import gleam/otp/actor.{StartResult}
import gleam/otp/process.{Pid}
import gleam/erlang/atom.{Atom}

pub external type CowboyRequest

external fn cowboy_reply(
  Int,
  Map(String, Dynamic),
  BitBuilder,
  CowboyRequest,
) -> CowboyRequest =
  "cowboy_req" "reply"

external fn erlang_get_method(CowboyRequest) -> Dynamic =
  "cowboy_req" "method"

fn get_method(request) -> http.Method {
  request
  |> erlang_get_method
  |> http.method_from_dynamic
  |> result.unwrap(http.Get)
}

external fn erlang_get_headers(CowboyRequest) -> Map(String, String) =
  "cowboy_req" "headers"

fn get_headers(request) -> List(http.Header) {
  request
  |> erlang_get_headers
  |> map.to_list
}

external fn get_body(CowboyRequest) -> #(BitString, CowboyRequest) =
  "gleam_cowboy_native" "read_entire_body"

external fn erlang_get_scheme(CowboyRequest) -> String =
  "cowboy_req" "scheme"

fn get_scheme(request) -> http.Scheme {
  request
  |> erlang_get_scheme
  |> http.scheme_from_string
  |> result.unwrap(http.Http)
}

external fn erlang_get_query(CowboyRequest) -> String =
  "cowboy_req" "qs"

fn get_query(request) -> Option(String) {
  case erlang_get_query(request) {
    "" -> None
    query -> Some(query)
  }
}

external fn get_path(CowboyRequest) -> String =
  "cowboy_req" "path"

external fn get_host(CowboyRequest) -> String =
  "cowboy_req" "host"

external fn get_port(CowboyRequest) -> Int =
  "cowboy_req" "port"

fn proplist_get_all(input: List(#(a, b)), key: a) -> List(b) {
  list.filter_map(
    input,
    fn(item) {
      case item {
        #(k, v) if k == key -> Ok(v)
        _ -> Error(Nil)
      }
    },
  )
}

// In cowboy all header values are strings except set-cookie, which is a
// list. This list has a special-case in Cowboy so we need to set it
// correctly.
// https://github.com/gleam-lang/cowboy/issues/3
fn cowboy_format_headers(headers: List(Header)) -> Map(String, Dynamic) {
  let set_cookie_headers = proplist_get_all(headers, "set-cookie")
  headers
  |> list.map(pair.map_second(_, dynamic.from))
  |> map.from_list
  |> map.insert("set-cookie", dynamic.from(set_cookie_headers))
}

fn cowboy_request_to_request(request) {
  let #(body, request) = get_body(request)

  Request(
    body: body,
    headers: get_headers(request),
    host: get_host(request),
    method: get_method(request),
    path: get_path(request),
    port: Some(get_port(request)),
    query: get_query(request),
    scheme: get_scheme(request),
  )
}

pub fn service_to_handler(
  service: Service(BitString, BitBuilder),
) -> fn(CowboyRequest) -> CowboyRequest {
  fn(request) {
    let response =
      request
      |> cowboy_request_to_request
      |> service
    let headers = cowboy_format_headers(response.headers)
    cowboy_reply(response.status, headers, response.body, request)
  }
}

/// Response returned from a websocket service. The response can either be a
/// normal HTTP response OR it can be a directive to upgrade to a persistant
/// websocket connection with the initial state for the socket
pub type WSResponse(out, state) {
  Upgrade(CowboyRequest, state)
  Respond(Response(out))
}

pub type WSService(in, out, state) =
  fn(Request(in)) -> WSResponse(out, state)

pub fn ws_service_to_handler(
  service: WSService(BitString, BitBuilder, state),
) -> fn(CowboyRequest) -> Dynamic {
  fn(request) {
    case service(cowboy_request_to_request(request)) {
      Respond(response) -> {
        let headers = cowboy_format_headers(response.headers)
        dynamic.from(cowboy_reply(
          response.status,
          headers,
          response.body,
          request,
        ))
      }
      Upgrade(request, state) ->
        dynamic.from(#(atom.from_string("cowboy_websocket"), request, state))
    }
  }
}
