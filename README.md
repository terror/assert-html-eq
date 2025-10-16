## assert-html-eq

[![CI](https://github.com/terror/assert-html-eq/actions/workflows/ci.yaml/badge.svg)](https://github.com/terror/assert-html-eq/actions/workflows/ci.yaml)

**assert-html-eq** is a tiny Rust crate offering a single macro, `assert_html_eq!`,
that performs semantic equality checks on HTML.

It parses both inputs into DOM trees and ignores irrelevant differences such as whitespace,
comment nodes, attribute ordering, and boolean attribute forms.
