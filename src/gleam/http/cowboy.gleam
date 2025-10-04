import gleam/bytes_tree.{type BytesTree}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/http.{type Header}
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/list
import gleam/option.{type Option, None, Some}
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
  body: BytesTree,
  request: CowboyRequest,
) -> CowboyRequest

@external(erlang, "cowboy_req", "method")
fn erlang_get_method(request: CowboyRequest) -> String

fn get_method(request) -> http.Method {
  request
  |> erlang_get_method
  |> http.parse_method
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

// In cowboy all header values are strings except set-cookie, which is a
// list. This list has a special-case in Cowboy so we need to set it
// correctly.
// https://github.com/gleam-lang/cowboy/issues/3
fn cowboy_format_headers(headers: List(Header)) -> Dict(String, Dynamic) {
  let set_cookie_headers = list.key_filter(headers, "set-cookie")

  dict.new()
  |> list.fold(over: headers, with: fn(headers, header) {
    let #(key, value) = header
    dict.insert(headers, key, to_dynamic(value))
  })
  |> dict.insert("set-cookie", to_dynamic(set_cookie_headers))
}

@external(erlang, "gleam_cowboy_native", "to_dynamic")
fn to_dynamic(value: any) -> Dynamic

fn service_to_handler(
  service: fn(Request(BitArray)) -> Response(BytesTree),
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
  service: fn(Request(BitArray)) -> Response(BytesTree),
  on_port number: Int,
) -> Result(Pid, Dynamic) {
  service
  |> service_to_handler
  |> erlang_start_link(number)
}
