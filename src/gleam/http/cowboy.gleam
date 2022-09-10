import gleam/list
import gleam/pair
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/result
import gleam/http.{Header}
import gleam/http/service.{Service}
import gleam/http/request.{Request}
import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{Dynamic}
import gleam/erlang/process.{Pid}

external type CowboyRequest

external fn erlang_start_link(
  handler: fn(CowboyRequest) -> CowboyRequest,
  port: Int,
) -> Result(Pid, Dynamic) =
  "gleam_cowboy_native" "start_link"

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

fn service_to_handler(
  service: Service(BitString, BitBuilder),
) -> fn(CowboyRequest) -> CowboyRequest {
  fn(request) {
    let #(body, request) = get_body(request)
    let response =
      service(Request(
        body: body,
        headers: get_headers(request),
        host: get_host(request),
        method: get_method(request),
        path: get_path(request),
        port: Some(get_port(request)),
        query: get_query(request),
        scheme: get_scheme(request),
      ))
    let status = response.status

    let headers = cowboy_format_headers(response.headers)
    let body = response.body
    cowboy_reply(status, headers, body, request)
  }
}

// TODO: document
// TODO: test
pub fn start(
  service: Service(BitString, BitBuilder),
  on_port number: Int,
) -> Result(Pid, Dynamic) {
  service
  |> service_to_handler
  |> erlang_start_link(number)
}
