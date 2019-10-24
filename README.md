# Cymbal

Yet another Rust implementation of the Monkey language from "Writing an Interpreter in Go" and "Writing a Compiler in Go."

[![CircleCI](https://circleci.com/gh/shuhei/cymbal.svg?style=svg)](https://circleci.com/gh/shuhei/cymbal)

## Development

Start REPL:

```sh
# Run in eval mode to directly evaluate AST
cargo run -- repl
# or
cargo run -- repl --eval

# Run in compile mode to compile AST into bytecode and execute it on VM
cargo run -- repl --compile
```

Build:

```sh
cargo build
```

Test:

```sh
cargo test
```

Benchmark with recursive fibonacci:

```sh
cargo run --release -- benchmark --eval
cargo run --release -- benchmark --compile
```

## License

MIT
