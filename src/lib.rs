use {
  scraper::{ElementRef, Html, Node},
  std::{borrow::Cow, fmt::Write},
  unicode_normalization::{UnicodeNormalization, is_nfc},
};

#[macro_export]
macro_rules! assert_html_eq {
  ($actual:expr, $expected:expr $(,)?) => {{
    $crate::__private::assert_html_eq(
      &$crate::__private::into_html($actual),
      &$crate::__private::into_html($expected),
      None
    );
  }};
  ($actual:expr, $expected:expr, $($arg:tt)+) => {{
    $crate::__private::assert_html_eq(
      &$crate::__private::into_html($actual),
      &$crate::__private::into_html($expected),
      Some(format!($($arg)+)),
    );
  }};
}

const BOOLEAN_ATTRIBUTES: &[&str] = &[
  "allowfullscreen",
  "async",
  "autofocus",
  "autoplay",
  "checked",
  "controls",
  "default",
  "defer",
  "disabled",
  "formnovalidate",
  "hidden",
  "inert",
  "ismap",
  "itemscope",
  "loop",
  "multiple",
  "muted",
  "nomodule",
  "novalidate",
  "open",
  "playsinline",
  "readonly",
  "required",
  "reversed",
  "selected",
  "truespeed",
];

const INLINE_ELEMENTS: &[&str] = &[
  "a", "abbr", "b", "bdi", "bdo", "cite", "code", "data", "del", "dfn", "em",
  "i", "img", "ins", "kbd", "label", "mark", "q", "s", "samp", "small", "span",
  "strong", "sub", "sup", "time", "u", "var",
];

const SPACE_MARKER: &str = "\u{2423}";

const TOKEN_SET_ATTRIBUTES: &[&str] = &["class", "part", "rel", "sandbox"];

const VOID_ELEMENTS: &[&str] = &[
  "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
  "param", "source", "track", "wbr",
];

const WHITESPACE_SIGNIFICANT_ELEMENTS: &[&str] =
  &["code", "pre", "script", "style", "textarea"];

struct Attribute<'a> {
  name: &'a str,
  value: Option<Cow<'a, str>>,
}

struct Normalizer<'a> {
  buffer: String,
  stack: Vec<&'a str>,
}

impl<'a> Normalizer<'a> {
  fn escape_attribute(input: &str) -> String {
    Self::escape_text(input).replace('"', "&quot;")
  }

  fn escape_text(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len());

    for ch in input.chars() {
      match ch {
        '&' => escaped.push_str("&amp;"),
        '<' => escaped.push_str("&lt;"),
        '>' => escaped.push_str("&gt;"),
        _ => escaped.push(ch),
      }
    }

    escaped
  }

  fn is_inline(node: &Node) -> bool {
    matches!(node, Node::Element(element) if INLINE_ELEMENTS.contains(&element.name()))
  }

  fn normalize(html: &'a Html) -> String {
    let mut normalizer = Normalizer {
      buffer: String::new(),
      stack: Vec::new(),
    };

    normalizer.write_element(&html.root_element(), 0);

    normalizer
      .buffer
      .truncate(normalizer.buffer.trim_end().len());

    normalizer.buffer
  }

  fn normalize_attributes(element: &ElementRef<'a>) -> Vec<Attribute<'a>> {
    let mut attrs = element
      .value()
      .attrs()
      .map(|(name, value)| {
        if TOKEN_SET_ATTRIBUTES.contains(&name) {
          Attribute {
            name,
            value: Some(Cow::Owned(Self::normalize_token_set(name, value))),
          }
        } else if BOOLEAN_ATTRIBUTES.contains(&name) {
          Attribute { name, value: None }
        } else {
          Attribute {
            name,
            value: Some(Cow::Borrowed(value)),
          }
        }
      })
      .collect::<Vec<_>>();

    attrs.sort_by(|a, b| a.name.cmp(b.name));
    attrs
  }

  fn normalize_text(text: &str, significant: bool) -> Cow<'_, str> {
    if significant {
      return Cow::Borrowed(text);
    }

    let unicode = if is_nfc(text) {
      Cow::Borrowed(text)
    } else {
      Cow::Owned(text.nfc().collect())
    };

    let collapsed = unicode
      .split(|c: char| c.is_whitespace() || c == '\u{00A0}')
      .filter(|s| !s.is_empty())
      .collect::<Vec<_>>()
      .join(" ");

    Cow::Owned(collapsed)
  }

  fn normalize_token_set(name: &str, value: &str) -> String {
    let lowercase = matches!(name, "part" | "rel" | "sandbox");

    let mut tokens = value
      .split_ascii_whitespace()
      .map(|token| {
        if lowercase {
          token.to_ascii_lowercase()
        } else {
          token.to_string()
        }
      })
      .collect::<Vec<_>>();

    tokens.sort_unstable();
    tokens.dedup();
    tokens.join(" ")
  }

  fn significant(&self) -> bool {
    self
      .stack
      .iter()
      .any(|tag| WHITESPACE_SIGNIFICANT_ELEMENTS.contains(tag))
  }

  fn write_element(&mut self, element: &ElementRef<'a>, depth: usize) {
    let indent = "  ".repeat(depth);

    self.write_open_tag(element, &indent);

    let name = element.value().name();

    let children = element
      .children()
      .filter(|child| {
        !matches!(
          child.value(),
          Node::Comment(_) | Node::Document | Node::Fragment
        )
      })
      .collect::<Vec<_>>();

    let has_visible_children =
      children.iter().any(|child| match child.value() {
        Node::Text(text) => {
          !Self::normalize_text(text, self.significant()).is_empty()
        }
        Node::Element(_)
        | Node::Doctype(_)
        | Node::ProcessingInstruction(_) => true,
        _ => false,
      });

    if !has_visible_children {
      self.write_empty_element_end(name);
      return;
    }

    self.buffer.push('>');

    let child_indent = "  ".repeat(depth + 1);

    let mut remaining = children.as_slice();

    while let Some((child, tail)) = remaining.split_first() {
      match child.value() {
        Node::Element(_) => {
          if !self.buffer.ends_with('\n') {
            self.buffer.push('\n');
          }

          self.write_element(
            &ElementRef::wrap(*child).expect("child must be an element"),
            depth + 1,
          );

          remaining = tail;
        }
        Node::Text(_) => {
          let text_run_len = 1
            + tail
              .iter()
              .take_while(|sibling| matches!(sibling.value(), Node::Text(_)))
              .count();

          let start = children.len() - remaining.len();

          let (text_run, rest) = remaining.split_at(text_run_len);

          let raw = text_run
            .iter()
            .filter_map(|n| match n.value() {
              Node::Text(t) => Some(&**t),
              _ => None,
            })
            .collect::<String>();

          let previous = start.checked_sub(1).and_then(|i| children.get(i));
          let next = children.get(start + text_run_len);

          self.write_text(
            &raw,
            &child_indent,
            previous.is_some_and(|node| Self::is_inline(node.value())),
            next.is_some_and(|node| Self::is_inline(node.value())),
          );

          remaining = rest;
        }
        Node::Doctype(doctype) => {
          self.buffer.push('\n');
          self.buffer.push_str(&child_indent);
          let _ = write!(self.buffer, "{doctype:?}");
          remaining = tail;
        }
        Node::ProcessingInstruction(pi) => {
          self.buffer.push('\n');
          self.buffer.push_str(&child_indent);
          let _ = write!(self.buffer, "{pi:?}");
          remaining = tail;
        }
        Node::Document | Node::Fragment | Node::Comment(_) => {
          remaining = tail;
        }
      }
    }

    if !self.buffer.ends_with('\n') {
      self.buffer.push('\n');
    }

    self.buffer.push_str(&indent);
    self.buffer.push_str("</");
    self.buffer.push_str(name);
    self.buffer.push('>');
    self.buffer.push('\n');

    self.stack.pop();
  }

  fn write_empty_element_end(&mut self, name: &str) {
    if VOID_ELEMENTS.contains(&name) {
      self.buffer.push_str(">\n");
    } else {
      self.buffer.push_str("></");
      self.buffer.push_str(name);
      self.buffer.push_str(">\n");
    }

    self.stack.pop();
  }

  fn write_open_tag(&mut self, element: &ElementRef<'a>, indent: &str) {
    let name = element.value().name();

    self.buffer.push_str(indent);
    self.buffer.push('<');
    self.buffer.push_str(name);

    for Attribute { name: key, value } in Self::normalize_attributes(element) {
      self.buffer.push(' ');
      self.buffer.push_str(key);

      if let Some(value) = value {
        self.buffer.push_str("=\"");
        self
          .buffer
          .push_str(&Self::escape_attribute(value.as_ref()));
        self.buffer.push('"');
      }
    }

    self.stack.push(name);
  }

  fn write_text(
    &mut self,
    raw: &str,
    indent: &str,
    after_inline: bool,
    before_inline: bool,
  ) {
    let leading_space = after_inline && raw.starts_with(char::is_whitespace);

    let trailing_space = before_inline && raw.ends_with(char::is_whitespace);

    let significant = self.significant();

    let joined = Self::normalize_text(raw, significant);

    let joined = if significant {
      joined
    } else if joined.is_empty() {
      if leading_space && trailing_space {
        Cow::Borrowed(SPACE_MARKER)
      } else {
        joined
      }
    } else {
      Cow::Owned(format!(
        "{}{joined}{}",
        if leading_space { SPACE_MARKER } else { "" },
        if trailing_space { SPACE_MARKER } else { "" },
      ))
    };

    if !joined.is_empty() {
      if !self.buffer.ends_with('\n') {
        self.buffer.push('\n');
      }

      self.buffer.push_str(indent);
      self.buffer.push_str(&Self::escape_text(&joined));
    }
  }
}

#[doc(hidden)]
pub mod __private {
  use {super::Normalizer, scraper::Html};

  pub trait IntoHtml {
    fn into_html(self) -> Html;
  }

  impl IntoHtml for Html {
    fn into_html(self) -> Html {
      self
    }
  }

  impl IntoHtml for &Html {
    fn into_html(self) -> Html {
      self.clone()
    }
  }

  impl IntoHtml for &str {
    fn into_html(self) -> Html {
      Html::parse_document(self)
    }
  }

  impl IntoHtml for String {
    fn into_html(self) -> Html {
      Html::parse_document(&self)
    }
  }

  pub fn assert_html_eq(
    actual: &Html,
    expected: &Html,
    custom: Option<String>,
  ) {
    let (actual, expected) = (
      Normalizer::normalize(actual),
      Normalizer::normalize(expected),
    );

    if actual == expected {
      return;
    }

    let diff = pretty_assertions::StrComparison::new(&actual, &expected);

    match custom {
      Some(message) => panic!("{message}\nassert_html_eq! failed:\n{diff}"),
      None => panic!("assert_html_eq! failed:\n{diff}"),
    }
  }

  pub fn into_html<T: IntoHtml>(input: T) -> Html {
    input.into_html()
  }
}

#[cfg(test)]
mod tests {
  use {super::*, indoc::indoc, scraper::Html};

  #[test]
  fn accepts_html_values() {
    assert_html_eq!(
      Html::parse_document("<p>Text</p>"),
      Html::parse_document("<p>Text</p>")
    );
  }

  #[test]
  fn attribute_order_is_ignored() {
    assert_html_eq!(
      "<img alt=\"x\" src=\"/a.png\">",
      "<img src=\"/a.png\" alt=\"x\">"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn attribute_quotes_are_escaped() {
    assert_html_eq!("<div a='x\" b=\"y'></div>", "<div a=\"x\" b=\"y\"></div>");
  }

  #[test]
  fn attribute_quoting_style_is_ignored() {
    assert_html_eq!(
      "<div title=\"Tom &amp; Jerry\"></div>",
      "<div title='Tom &amp; Jerry'></div>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn attribute_whitespace_is_preserved_for_non_token_sets() {
    assert_html_eq!(
      "<div title=\"hello  world\"></div>",
      "<div title=\"hello world\"></div>"
    );
  }

  #[test]
  fn boolean_attribute_forms_are_equivalent() {
    assert_html_eq!("<input disabled>", "<input disabled=\"\">");
    assert_html_eq!("<input disabled>", "<input disabled=disabled>");
  }

  #[test]
  fn coalesces_adjacent_text_nodes() {
    assert_html_eq!("<p>Hello<!-- comment --> world</p>", "<p>Hello world</p>");
  }

  #[test]
  fn code_entities_compare_equal() {
    assert_html_eq!(
      "<code>&quot;test&quot; &amp; 'foo'</code>",
      "<code>\"test\" &amp; 'foo'</code>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn code_whitespace_matters() {
    assert_html_eq!("<code>fn  main(){}</code>", "<code>fn main(){}</code>");
  }

  #[test]
  fn collapses_leading_and_trailing_spaces_across_text_runs() {
    assert_html_eq!("<p> A   B </p>", "<p>A B</p>");
    assert_html_eq!("<p>A&nbsp;&nbsp;B</p>", "<p>A B</p>");
  }

  #[test]
  fn comment_boundaries_between_text_runs_not_significant() {
    assert_html_eq!(
      "<p>Hello<!--a--> \n <!--b-->world</p>",
      "<p>Hello world</p>"
    );
  }

  #[test]
  fn compares_html_strings() {
    assert_html_eq!(
      "<div><span>Hello</span></div>",
      "<div>
        <span>
          Hello
        </span>
      </div>",
    );
  }

  #[test]
  #[should_panic(
    expected = "\u{1b}[31m<      \u{1b}[0m\u{1b}[1;48;5;52;31mfoo"
  )]
  fn diff_shows_actual_as_left() {
    assert_html_eq!("<p>foo</p>", "<p>bar</p>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn different_child_order_is_detected() {
    assert_html_eq!(
      "<ul><li>1</li><li>2</li></ul>",
      "<ul><li>2</li><li>1</li></ul>"
    );
  }

  #[test]
  fn doctype_is_ignored_for_semantics() {
    assert_html_eq!("<!doctype html><div>ok</div>", "<div>ok</div>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn does_not_normalize_unicode_in_significant_whitespace() {
    assert_html_eq!("<pre>Café</pre>", "<pre>Cafe\u{0301}</pre>");
  }

  #[test]
  fn drops_whitespace_only_children_in_nonsignificant_contexts() {
    assert_html_eq!("<div>   \n\t  </div>", "<div></div>");
  }

  #[test]
  fn duplicate_attributes_collapse_to_parser_behavior() {
    assert_html_eq!("<img alt=\"a\" alt=\"b\">", "<img alt=\"a\">");
  }

  #[test]
  fn duplicate_tokens_in_token_set_attrs_are_deduped() {
    assert_html_eq!(
      "<link rel=\"preload preload stylesheet\">",
      "<link rel=\"stylesheet preload\">"
    );

    assert_html_eq!(
      "<iframe sandbox=\"allow-scripts allow-scripts allow-same-origin\">",
      "<iframe sandbox=\"allow-same-origin allow-scripts\">"
    );

    assert_html_eq!("<div class=\"a  a   b\">", "<div class=\"a b\">");
  }

  #[test]
  fn entities_in_text_and_attrs_compare_equal() {
    assert_html_eq!(
      "<p>5 &lt; 6 &amp;&amp; 7 &gt; 6</p>",
      "<p>5 < 6 && 7 > 6</p>"
    );

    assert_html_eq!(
      "<a href=\"/?a=1&amp;b=2\">x</a>",
      "<a href='/?a=1&b=2'>x</a>"
    );
  }

  #[test]
  fn html_case_insensitivity_normalizes() {
    assert_html_eq!("<DIV CLASS=\"B A\"></DIV>", "<div class=\"A B\"></div>");
  }

  #[test]
  fn ignores_comments() {
    assert_html_eq!(
      "<div><!-- noisy --><span>Text</span></div>",
      "<div><span>Text</span></div>"
    );
  }

  #[test]
  fn inline_element_spacing_amount_is_ignored() {
    assert_html_eq!(
      "<p>foo \n  <b>bar</b>\n baz</p>",
      "<p>foo <b>bar</b> baz</p>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn inline_element_spacing_matters() {
    assert_html_eq!("<p>foo<b>bar</b></p>", "<p>foo <b>bar</b></p>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn inter_inline_whitespace_matters() {
    assert_html_eq!(
      "<p><b>foo</b><i>bar</i></p>",
      "<p><b>foo</b> <i>bar</i></p>"
    );
  }

  #[test]
  fn leading_trailing_whitespace_inside_block_is_ignored() {
    assert_html_eq!("<div>\n   hello  \n</div>", "<div>hello</div>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn mismatch_panics() {
    assert_html_eq!("<div></div>", "<span></span>");
  }

  #[test]
  fn mixed_adjacent_text_and_nodes_are_coalesced_correctly() {
    assert_html_eq!("<p>a<!--x-->  b<!--y-->   c</p>", "<p>a b c</p>");
  }

  #[test]
  fn mixed_text_runs_around_inline_elements() {
    assert_html_eq!("<p> A <em>  B  </em>  C </p>", "<p>A <em>B</em> C</p>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn nbsp_differs_from_space_in_significant_contexts() {
    assert_html_eq!("<pre>A\u{00A0}B</pre>", "<pre>A B</pre>");
  }

  #[test]
  fn nbsp_entity_and_char_are_equivalent_outside_significant_contexts() {
    assert_html_eq!("<p>A&nbsp;B</p>", "<p>A\u{00A0}B</p>");
  }

  #[test]
  fn nested_structure_with_whitespace_noise() {
    assert_html_eq!(
      "<div>\n  <ul>\n    <li> 1 </li>\n    <li>  2</li>\n  </ul>\n</div>",
      "<div><ul><li>1</li><li>2</li></ul></div>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn non_boolean_enumerated_attrs_are_not_collapsed() {
    assert_html_eq!("<div contenteditable=\"false\"></div>", "<div></div>");
  }

  #[test]
  fn normalize_html_escapes_text_content() {
    let html = Html::parse_document(
      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body><p>5 < \
       6 && 7 > 6</p></body></html>",
    );

    let normalized = Normalizer::normalize(&html);

    let expected = indoc! {r#"
      <html>
        <head>
          <meta charset="utf-8">
        </head>
        <body>
          <p>
            5 &lt; 6 &amp;&amp; 7 &gt; 6
          </p>
        </body>
      </html>"#};

    assert_eq!(normalized, expected);
  }

  #[test]
  fn normalize_html_reconstructs_document_structure() {
    let html = Html::parse_document(
      "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title> \
       Example </title></head><body><main><h1> Hello </h1><p>Test</p></main></body></html>",
    );

    let normalized = Normalizer::normalize(&html);

    let expected = indoc! {r#"
      <html lang="en">
        <head>
          <meta charset="utf-8">
          <title>
            Example
          </title>
        </head>
        <body>
          <main>
            <h1>
              Hello
            </h1>
            <p>
              Test
            </p>
          </main>
        </body>
      </html>"#};

    assert_eq!(normalized, expected);
  }

  #[test]
  fn normalizes_boolean_attributes() {
    assert_html_eq!(
      "<input type=\"checkbox\" disabled=\"disabled\" checked=\"\" />",
      "<input type=\"checkbox\" disabled checked>"
    );
  }

  #[test]
  fn normalizes_class_token_order() {
    assert_html_eq!(
      "<div class=\"b a c b\"></div>",
      "<div class=\"a b c\"></div>"
    );
  }

  #[test]
  fn normalizes_nbsp_as_space() {
    assert_html_eq!("<p>A&nbsp;B</p>", "<p>A B</p>");
  }

  #[test]
  fn normalizes_other_token_set_attributes() {
    assert_html_eq!(
      "<link rel=\"stylesheet preload\">",
      "<link rel=\"preload stylesheet\">"
    );

    assert_html_eq!(
      "<iframe sandbox=\"allow-same-origin allow-scripts\"></iframe>",
      "<iframe sandbox=\"allow-scripts allow-same-origin\"></iframe>"
    );

    assert_html_eq!(
      "<div part=\"card badge\"></div>",
      "<div part=\"badge card\"></div>"
    );

    assert_html_eq!(
      "<link rel=\"Preload StyleSheet\">",
      "<link rel=\"stylesheet preload\">"
    );

    assert_html_eq!(
      "<iframe sandbox=\"Allow-Same-Origin ALLOW-SCRIPTS\"></iframe>",
      "<iframe sandbox=\"allow-scripts allow-same-origin\"></iframe>"
    );
  }

  #[test]
  fn normalizes_unicode_text() {
    assert_html_eq!("<p>Café</p>", "<p>Cafe\u{0301}</p>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_pre_whitespace() {
    assert_html_eq!(
      "<pre>line  with  spaces</pre>",
      "<pre>line with spaces</pre>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_script_whitespace() {
    assert_html_eq!(
      "<script>if (x) {\n  doThing();\n}</script>",
      "<script>if (x) { doThing(); }</script>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_style_attribute_verbatim() {
    assert_html_eq!(
      "<div style=\"color: red;  margin:0\"></div>",
      "<div style=\"color:red;margin:0\"></div>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_style_whitespace() {
    assert_html_eq!(
      "<style>body {\n  color: red;\n}</style>",
      "<style>body { color: red; }</style>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_textarea_whitespace() {
    assert_html_eq!(
      "<textarea>line  with  spaces</textarea>",
      "<textarea>line with spaces</textarea>"
    );
  }

  #[test]
  fn self_closing_non_void_normalizes() {
    assert_html_eq!("<div/>", "<div></div>");
  }

  #[test]
  fn spacing_around_inline_elements_is_semantic() {
    assert_html_eq!("<p>a <em>b</em> c</p>", "<p>a <em>b</em> c</p>");
    assert_html_eq!("<p>a<em>b</em>c</p>", "<p>a<em>b</em>c</p>");
  }

  #[test]
  fn template_content_is_compared_if_supported() {
    assert_html_eq!(
      "<template><span> A   B </span></template>",
      "<template><span>A B</span></template>"
    );
  }

  #[test]
  fn treats_void_end_tags_equivalently() {
    assert_html_eq!("<input disabled>", "<input disabled></input>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn unicode_normalization_isnt_applied_in_significant_contexts() {
    assert_html_eq!(
      "<script>const name = 'Cafe\u{0301}';</script>",
      "<script>const name = 'Café';</script>"
    );
  }

  #[test]
  fn unusual_whitespace_chars_collapse() {
    assert_html_eq!("<p>A\u{2009}\u{2003}B</p>", "<p>A B</p>");
  }

  #[test]
  fn void_element_syntax_variants_are_equivalent() {
    assert_html_eq!("<br>", "<br/>");
    assert_html_eq!("<img src=x>", "<img src=\"x\"/>");
  }

  #[test]
  fn whitespace_around_comments_in_significant_contexts_isnt_invented() {
    assert_html_eq!("<pre>foo<!--x-->bar</pre>", "<pre>foobar</pre>");
  }

  #[test]
  fn whitespace_only_text_runs_between_elements_are_ignored() {
    assert_html_eq!(
      "<div>a</div>\n   \t  <div>b</div>",
      "<div>a</div><div>b</div>"
    );
  }
}
