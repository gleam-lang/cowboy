# Gleam Cowboy! 🤠

A Gleam HTTP service adapter for the [Cowboy][cowboy] web server.

You may want to consider using the [Mist][mist] web server instead as it has
better performance, features type safe websockets, and is written entirely in
Gleam. It is also supported by the [Wisp][wisp] web framework.

## Installation

```sh
gleam add gleam_cowboy
```

```gleam
import gleam/erlang/process
import gleam/http/cowboy
import gleam/http/response.{type Response}
import gleam/http/request.{type Request}
import gleam/bytes_builder.{type BytesBuilder}

// Define a HTTP service
//
pub fn my_service(request: Request(t)) -> Response(BytesBuilder) {
  let body = bytes_builder.from_string("Hello, world!")

  response.new(200)
  |> response.prepend_header("made-with", "Gleam")
  |> response.set_body(body)
}

// Start it on port 3000!
//
pub fn main() {
  cowboy.start(my_service, on_port: 3000)
  process.sleep_forever()
}
```

## Limitations

Cowboy does not support duplicate HTTP headers so any duplicates specified by
the Gleam HTTP service will not be sent to the client.

[cowboy]: https://github.com/ninenines/cowboy
[mist]: https://github.com/rawhat/mist
[wisp]: https://github.com/lpil/wisp
