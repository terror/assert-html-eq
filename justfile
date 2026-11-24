set dotenv-load

export EDITOR := 'nvim'

alias f := fmt
alias t := test

default:
  just --list

ci: fmt-check clippy test

clippy:
  cargo clippy

fmt:
  cargo fmt

fmt-check:
  cargo fmt -- --check

test:
  cargo test
