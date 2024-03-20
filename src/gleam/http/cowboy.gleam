import gleam/bytes_builder.{type BytesBuilder}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/http.{type Header}
import gleam/http/request.{Request}
import gleam/http/service.{type Service}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

type CowboyRequest

@external(erlang, "gleam_cowboy_native", "start_link")
fn erlang_start_link(
  handler: fn(CowboyRequest) -> CowboyRequest,
  port: Int,
) -> Result(Pid, Dynamic)

@external(erlang, "gleam_cowboy_native", "set_headers")
fn erlang_set_headers(
  headers: Dict(String, Dynamic),
  request: CowboyRequest,
) -> CowboyRequest

fn set_headers(
  headers: Dict(String, Dynamic),
  request: CowboyRequest,
) -> CowboyRequest {
  erlang_set_headers(headers, request)
}

@external(erlang, "cowboy_req", "reply")
fn cowboy_reply(
  status: Int,
  headers: Dict(String, Dynamic),
  body: BytesBuilder,
  request: CowboyRequest,
) -> CowboyRequest

@external(erlang, "cowboy_req", "method")
fn erlang_get_method(request: CowboyRequest) -> Dynamic

fn get_method(request) -> http.Method {
  request
  |> erlang_get_method
  |> http.method_from_dynamic
  |> result.unwrap(http.Get)
}

@external(erlang, "cowboy_req", "headers")
fn erlang_get_headers(request: CowboyRequest) -> Dict(String, String)

fn get_headers(request) -> List(http.Header) {
  request
  |> erlang_get_headers
  |> dict.to_list
}

@external(erlang, "gleam_cowboy_native", "read_entire_body")
fn get_body(request: CowboyRequest) -> #(BitArray, CowboyRequest)

@external(erlang, "cowboy_req", "scheme")
fn erlang_get_scheme(request: CowboyRequest) -> String

fn get_scheme(request) -> http.Scheme {
  request
  |> erlang_get_scheme
  |> http.scheme_from_string
  |> result.unwrap(http.Http)
}

@external(erlang, "cowboy_req", "qs")
fn erlang_get_query(request: CowboyRequest) -> String

fn get_query(request) -> Option(String) {
  case erlang_get_query(request) {
    "" -> None
    query -> Some(query)
  }
}

@external(erlang, "cowboy_req", "path")
fn get_path(request: CowboyRequest) -> String

@external(erlang, "cowboy_req", "host")
fn get_host(request: CowboyRequest) -> String

@external(erlang, "cowboy_req", "port")
fn get_port(request: CowboyRequest) -> Int

fn proplist_get_all(input: List(#(a, b)), key: a) -> List(b) {
  list.filter_map(input, fn(item) {
    case item {
      #(k, v) if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}

// In cowboy all header values are strings except set-cookie, which is a
// list. This list has a special-case in Cowboy so we need to set it
// correctly.
// https://github.com/gleam-lang/cowboy/issues/3
fn cowboy_format_headers(headers: List(Header)) -> Dict(String, Dynamic) {
  let set_cookie_headers = proplist_get_all(headers, "set-cookie")
  headers
  |> list.map(pair.map_second(_, dynamic.from))
  |> dict.from_list
  |> dict.insert("set-cookie", dynamic.from(set_cookie_headers))
}

fn service_to_handler(
  service: Service(BitArray, BytesBuilder),
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
    // We set headers directly on the CowboyRequest as we cannot set cookie headers
    // using cowboy_req:set_resp_headers as of 2.11.0.
    // https://github.com/ninenines/cowboy/pull/1624#issuecomment-1915324578
    let request = set_headers(headers, request)
    let body = response.body
    cowboy_reply(status, dict.new(), body, request)
  }
}

// TODO: document
// TODO: test
pub fn start(
  service: Service(BitArray, BytesBuilder),
  on_port number: Int,
) -> Result(Pid, Dynamic) {
  service
  |> service_to_handler
  |> erlang_start_link(number)
}
