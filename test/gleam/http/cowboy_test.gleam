import gleam/http/cowboy
import gleam/bit_builder.{BitBuilder}
import gleam/http.{Get, Head, Post}
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/hackney

pub fn echo_service(request: Request(BitString)) -> Response(BitBuilder) {
  let body = case request.body {
    <<>> -> bit_builder.from_string("Default body")
    x -> bit_builder.from_bit_string(x)
  }
  response.new(200)
  |> response.prepend_header("made-with", "Gleam")
  |> response.set_body(body)
}

pub fn request_test() {
  // TODO: Assign these ports on random free ones aviable
  // TODO: Shut down server after test?
  let port = 3078
  assert Ok(_) = cowboy.start(echo_service, on_port: port)

  let req =
    request.new()
    |> request.set_method(Get)
    |> request.set_host("0.0.0.0")
    |> request.set_scheme(http.Http)
    |> request.set_port(port)

  assert Ok(resp) = hackney.send(req)
  assert 200 = resp.status
  assert Ok("Gleam") = response.get_header(resp, "made-with")
  assert "Default body" = resp.body
}

pub fn get_request_does_not_discard_body_test() {
  let port = 3079
  assert Ok(_) = cowboy.start(echo_service, on_port: port)

  let req =
    request.new()
    |> request.set_method(Get)
    |> request.set_host("0.0.0.0")
    |> request.set_scheme(http.Http)
    |> request.set_port(port)
    |> request.set_body("This does NOT get dropped")

  assert Ok(resp) = hackney.send(req)
  assert 200 = resp.status
  assert Ok("Gleam") = response.get_header(resp, "made-with")
  assert "This does NOT get dropped" = resp.body
}

pub fn head_request_discards_body_test() {
  let port = 3080
  assert Ok(_) = cowboy.start(echo_service, on_port: port)

  let req =
    request.new()
    |> request.set_method(Head)
    |> request.set_host("0.0.0.0")
    |> request.set_scheme(http.Http)
    |> request.set_port(port)
    |> request.set_body("This gets dropped")

  assert Ok(resp) = hackney.send(req)
  assert 200 = resp.status
  assert Ok("Gleam") = response.get_header(resp, "made-with")
  assert "" = resp.body
}

pub fn body_is_echoed_on_post_test() {
  let port = 3081
  assert Ok(_) = cowboy.start(echo_service, on_port: port)

  let req =
    request.new()
    |> request.set_method(Post)
    |> request.set_host("0.0.0.0")
    |> request.set_scheme(http.Http)
    |> request.set_port(port)
    |> request.set_body("Ping")

  assert Ok(resp) = hackney.send(req)
  assert 200 = resp.status
  assert Ok("Gleam") = response.get_header(resp, "made-with")
  assert "Ping" = resp.body
}
