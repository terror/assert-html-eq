set dotenv-load

export EDITOR := 'nvim'

alias f := fmt
alias t := test

ci: fmt-check clippy test

default:
  just --list

clippy:
  cargo clippy

fmt:
  cargo fmt

fmt-check:
  cargo fmt -- --check

test:
  cargo test
