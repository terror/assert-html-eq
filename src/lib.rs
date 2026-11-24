use {
  scraper::{ElementRef, Html, Node},
  std::borrow::Cow,
  unicode_normalization::{UnicodeNormalization, is_nfc},
};

pub use scraper;

#[macro_export]
macro_rules! assert_html_eq {
  ($actual:expr, $expected:expr $(,)?) => {{
    if let Some(diff) = $crate::__private::diff(
      &$crate::__private::into_html($actual),
      &$crate::__private::into_html($expected),
    ) {
      ::core::panic!("assert_html_eq! failed:\n{}", diff);
    }
  }};
  ($actual:expr, $expected:expr, $($arg:tt)+) => {{
    if let Some(diff) = $crate::__private::diff(
      &$crate::__private::into_html($actual),
      &$crate::__private::into_html($expected),
    ) {
      ::core::panic!(
        "{}\nassert_html_eq! failed:\n{}",
        ::core::format_args!($($arg)+),
        diff,
      );
    }
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

const DISPLAY_BLOCK_ELEMENTS: &[&str] = &[
  "address",
  "article",
  "aside",
  "blockquote",
  "body",
  "br",
  "caption",
  "center",
  "col",
  "colgroup",
  "dd",
  "details",
  "dialog",
  "dir",
  "div",
  "dl",
  "dt",
  "fieldset",
  "figcaption",
  "figure",
  "footer",
  "form",
  "frameset",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "head",
  "header",
  "hgroup",
  "hr",
  "html",
  "legend",
  "li",
  "listing",
  "main",
  "menu",
  "nav",
  "ol",
  "optgroup",
  "option",
  "p",
  "plaintext",
  "pre",
  "search",
  "section",
  "summary",
  "table",
  "tbody",
  "td",
  "tfoot",
  "th",
  "thead",
  "tr",
  "ul",
  "xmp",
];

const DISPLAY_INLINE_BLOCK_ELEMENTS: &[&str] = &[
  "audio", "button", "canvas", "embed", "iframe", "img", "input", "math",
  "meter", "object", "progress", "select", "svg", "textarea", "video",
];

const DISPLAY_NONE_ELEMENTS: &[&str] = &[
  "area", "base", "datalist", "link", "meta", "noscript", "param", "rp",
  "script", "source", "style", "template", "title", "track", "wbr",
];

const SPACE_MARKER: &str = "\u{2423}";

const TOKEN_SET_ATTRIBUTES: &[&str] = &["class", "part", "rel", "sandbox"];

const VOID_ELEMENTS: &[&str] = &[
  "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
  "param", "source", "track", "wbr",
];

const WHITESPACE_SIGNIFICANT_ELEMENTS: &[&str] = &[
  "code",
  "listing",
  "plaintext",
  "pre",
  "script",
  "style",
  "textarea",
  "xmp",
];

enum Token<'a> {
  Close(&'a str),
  Element(ElementRef<'a>),
  Open(ElementRef<'a>),
  Space,
  Text(String),
}

struct Attribute<'a> {
  name: Cow<'a, str>,
  value: Option<Cow<'a, str>>,
}

struct Normalizer {
  buffer: String,
  significant_depth: usize,
}

impl Normalizer {
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
        '\u{2423}' => escaped.push_str("&#x2423;"),
        _ => escaped.push(ch),
      }
    }

    escaped
  }

  fn flush_boundary<'a>(
    tokens: &mut Vec<Token<'a>>,
    boundary: &mut Vec<Token<'a>>,
    space: bool,
  ) {
    if space {
      let mut depth = 0isize;
      let mut lowest = 0;
      let mut position = 0;

      for (index, token) in boundary.iter().enumerate() {
        match token {
          Token::Open(_) => depth += 1,
          Token::Close(_) => depth -= 1,
          _ => {}
        }

        if depth < lowest {
          lowest = depth;
          position = index + 1;
        }
      }

      boundary.insert(position, Token::Space);
    }

    tokens.append(boundary);
  }

  fn inline_tokens<'a>(element: &ElementRef<'a>) -> Vec<Token<'a>> {
    let mut tokens = Vec::new();

    let mut stack = vec![(None, element.children())];

    while let Some((name, children)) = stack.last_mut() {
      let Some(child) = children.next() else {
        if let Some(name) = *name {
          tokens.push(Token::Close(name));
        }

        stack.pop();

        continue;
      };

      if let Some(element) = ElementRef::wrap(child) {
        let name = element.value().name();

        if Self::is_transparent(name) {
          tokens.push(Token::Open(element));
          stack.push((Some(name), child.children()));
        } else {
          tokens.push(Token::Element(element));
        }
      } else if let Node::Text(text) = child.value() {
        Self::push_text_tokens(text, &mut tokens);
      }
    }

    tokens
  }

  fn is_transparent(name: &str) -> bool {
    [
      DISPLAY_BLOCK_ELEMENTS,
      DISPLAY_INLINE_BLOCK_ELEMENTS,
      DISPLAY_NONE_ELEMENTS,
      VOID_ELEMENTS,
      WHITESPACE_SIGNIFICANT_ELEMENTS,
    ]
    .iter()
    .all(|elements| !elements.contains(&name))
  }

  fn normalize(html: &Html) -> String {
    let mut normalizer = Normalizer {
      buffer: String::new(),
      significant_depth: 0,
    };

    normalizer.write_element(&html.root_element(), 0);

    normalizer
      .buffer
      .truncate(normalizer.buffer.trim_end().len());

    normalizer.buffer
  }

  fn normalize_attributes<'a>(element: &ElementRef<'a>) -> Vec<Attribute<'a>> {
    let mut attrs = element
      .value()
      .attrs
      .iter()
      .map(|(name, value)| {
        let (name, value) = match &name.prefix {
          Some(prefix) => {
            (Cow::Owned(format!("{prefix}:{}", name.local)), &**value)
          }
          None => (Cow::Borrowed(&*name.local), &**value),
        };

        if TOKEN_SET_ATTRIBUTES.contains(&&*name) {
          let value = Self::normalize_token_set(&name, value);

          Attribute {
            name,
            value: Some(Cow::Owned(value)),
          }
        } else if name == "hidden" && value.eq_ignore_ascii_case("until-found")
        {
          Attribute {
            name,
            value: Some(Cow::Borrowed("until-found")),
          }
        } else if BOOLEAN_ATTRIBUTES.contains(&&*name) {
          Attribute { name, value: None }
        } else {
          Attribute {
            name,
            value: Some(Cow::Borrowed(value)),
          }
        }
      })
      .collect::<Vec<_>>();

    attrs.sort_by(|a, b| a.name.cmp(&b.name));
    attrs
  }

  fn normalize_token_set(name: &str, value: &str) -> String {
    let lowercase = matches!(name, "rel" | "sandbox");

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

  fn normalize_whitespace(tokens: Vec<Token<'_>>) -> Vec<Token<'_>> {
    let mut normalized = Vec::new();

    let mut boundary = Vec::new();

    let mut space = false;

    let mut inline = false;

    for token in tokens {
      match &token {
        Token::Space => space = true,
        Token::Close(_) | Token::Open(_) => boundary.push(token),
        Token::Element(element)
          if DISPLAY_NONE_ELEMENTS.contains(&element.value().name()) =>
        {
          boundary.push(token);
        }
        Token::Element(element)
          if DISPLAY_BLOCK_ELEMENTS.contains(&element.value().name()) =>
        {
          Self::flush_boundary(&mut normalized, &mut boundary, false);
          normalized.push(token);
          inline = false;
          space = false;
        }
        Token::Element(_) | Token::Text(_) => {
          Self::flush_boundary(&mut normalized, &mut boundary, space && inline);
          normalized.push(token);
          inline = true;
          space = false;
        }
      }
    }

    Self::flush_boundary(&mut normalized, &mut boundary, false);

    normalized
  }

  fn push_text_tokens(text: &str, tokens: &mut Vec<Token<'_>>) {
    let text = if is_nfc(text) {
      Cow::Borrowed(text)
    } else {
      Cow::Owned(text.nfc().collect::<String>())
    };

    for (index, word) in text.split(char::is_whitespace).enumerate() {
      if index > 0 {
        tokens.push(Token::Space);
      }

      if !word.is_empty() {
        tokens.push(Token::Text(word.to_owned()));
      }
    }
  }

  fn raw_tokens<'a>(element: &ElementRef<'a>) -> Vec<Token<'a>> {
    element
      .children()
      .filter_map(|child| match child.value() {
        Node::Text(text) if !text.is_empty() => {
          Some(Token::Text(text.to_string()))
        }
        Node::Element(_) => ElementRef::wrap(child).map(Token::Element),
        _ => None,
      })
      .collect()
  }

  fn significant(&self) -> bool {
    self.significant_depth > 0
  }

  fn write_element(&mut self, element: &ElementRef, depth: usize) {
    let name = element.value().name();

    let significant = WHITESPACE_SIGNIFICANT_ELEMENTS.contains(&name);

    self.significant_depth += usize::from(significant);

    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
      self.write_element_contents(element, depth);
    });

    self.significant_depth -= usize::from(significant);
  }

  fn write_element_contents(&mut self, element: &ElementRef, depth: usize) {
    let indent = "  ".repeat(depth);

    self.write_open_tag(element, &indent);

    let name = element.value().name();

    let tokens = if self.significant() {
      Self::raw_tokens(element)
    } else {
      Self::normalize_whitespace(Self::inline_tokens(element))
    };

    if tokens.is_empty() {
      self.write_empty_element_end(name);
      return;
    }

    self.buffer.push_str(">\n");

    self.write_tokens(&tokens, depth + 1, matches!(name, "script" | "style"));

    self.buffer.push_str(&indent);
    self.buffer.push_str("</");
    self.buffer.push_str(name);
    self.buffer.push('>');
    self.buffer.push('\n');
  }

  fn write_empty_element_end(&mut self, name: &str) {
    if VOID_ELEMENTS.contains(&name) {
      self.buffer.push_str(">\n");
    } else {
      self.buffer.push_str("></");
      self.buffer.push_str(name);
      self.buffer.push_str(">\n");
    }
  }

  fn write_open_tag(&mut self, element: &ElementRef, indent: &str) {
    let name = element.value().name();

    self.buffer.push_str(indent);
    self.buffer.push('<');
    self.buffer.push_str(name);

    for Attribute { name: key, value } in Self::normalize_attributes(element) {
      self.buffer.push(' ');
      self.buffer.push_str(&key);

      if let Some(value) = value {
        self.buffer.push_str("=\"");
        self
          .buffer
          .push_str(&Self::escape_attribute(value.as_ref()));
        self.buffer.push('"');
      }
    }
  }

  fn write_text(&mut self, run: &[Token], indent: &str, raw: bool) {
    self.buffer.push_str(indent);

    for (index, token) in run.iter().enumerate() {
      match token {
        Token::Space if index == 0 || index == run.len() - 1 => {
          self.buffer.push_str(SPACE_MARKER);
        }
        Token::Space => self.buffer.push(' '),
        Token::Text(text) if raw => self.buffer.push_str(text),
        Token::Text(text) => self.buffer.push_str(&Self::escape_text(text)),
        Token::Close(_) | Token::Element(_) | Token::Open(_) => {}
      }
    }

    self.buffer.push('\n');
  }

  fn write_tokens(&mut self, tokens: &[Token], mut depth: usize, raw: bool) {
    let mut remaining = tokens;

    while let Some((token, tail)) = remaining.split_first() {
      match token {
        Token::Close(name) => {
          depth -= 1;
          self.buffer.push_str(&"  ".repeat(depth));
          self.buffer.push_str("</");
          self.buffer.push_str(name);
          self.buffer.push_str(">\n");
          remaining = tail;
        }
        Token::Element(element) => {
          self.write_element(element, depth);
          remaining = tail;
        }
        Token::Open(element) => {
          self.write_open_tag(element, &"  ".repeat(depth));

          if let Some((Token::Close(name), rest)) = tail.split_first() {
            self.write_empty_element_end(name);
            remaining = rest;
          } else {
            self.buffer.push_str(">\n");
            depth += 1;
            remaining = tail;
          }
        }
        Token::Space | Token::Text(_) => {
          let length = remaining
            .iter()
            .take_while(|token| matches!(token, Token::Space | Token::Text(_)))
            .count();

          let (run, rest) = remaining.split_at(length);

          self.write_text(run, &"  ".repeat(depth), raw);

          remaining = rest;
        }
      }
    }
  }
}

#[doc(hidden)]
pub mod __private {
  use {super::Normalizer, scraper::Html, std::borrow::Cow};

  pub trait IntoHtml<'a> {
    fn into_html(self) -> Cow<'a, Html>;
  }

  impl IntoHtml<'_> for Html {
    fn into_html(self) -> Cow<'static, Html> {
      Cow::Owned(self)
    }
  }

  impl<'a> IntoHtml<'a> for &'a Html {
    fn into_html(self) -> Cow<'a, Html> {
      Cow::Borrowed(self)
    }
  }

  impl IntoHtml<'_> for &str {
    fn into_html(self) -> Cow<'static, Html> {
      Cow::Owned(Html::parse_document(self))
    }
  }

  impl IntoHtml<'_> for &String {
    fn into_html(self) -> Cow<'static, Html> {
      Cow::Owned(Html::parse_document(self))
    }
  }

  impl IntoHtml<'_> for String {
    fn into_html(self) -> Cow<'static, Html> {
      Cow::Owned(Html::parse_document(&self))
    }
  }

  #[must_use]
  pub fn diff(actual: &Html, expected: &Html) -> Option<String> {
    let (actual, expected) = (
      Normalizer::normalize(actual),
      Normalizer::normalize(expected),
    );

    (actual != expected).then(|| {
      pretty_assertions::StrComparison::new(&actual, &expected).to_string()
    })
  }

  pub fn into_html<'a, T: IntoHtml<'a>>(input: T) -> Cow<'a, Html> {
    let html = input.into_html();

    if html.tree.root().value().is_fragment() {
      Cow::Owned(Html::parse_document(&html.html()))
    } else {
      html
    }
  }
}

#[cfg(test)]
mod tests {
  use {super::*, indoc::indoc, scraper::Html, std::cell::Cell};

  #[test]
  fn accepts_html_fragments() {
    assert_html_eq!(Html::parse_fragment("<p>foo</p>"), "<p>foo</p>");
  }

  #[test]
  fn accepts_html_references() {
    assert_html_eq!(&Html::parse_document("<p>foo</p>"), "<p>foo</p>");
  }

  #[test]
  fn accepts_html_values() {
    assert_html_eq!(
      Html::parse_document("<p>Text</p>"),
      Html::parse_document("<p>Text</p>")
    );
  }

  #[test]
  fn accepts_string_references() {
    assert_html_eq!(&"<p>foo</p>".to_string(), "<p>foo</p>");
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
  #[should_panic(expected = "foo bar\nassert_html_eq! failed:\n")]
  fn custom_message_is_included() {
    assert_html_eq!("<p>baz</p>", "<p>qux</p>", "foo {}", "bar");
  }

  #[test]
  fn custom_message_is_lazy() {
    let evaluated = Cell::new(false);

    assert_html_eq!("<p>foo</p>", "<p>foo</p>", "{}", {
      evaluated.set(true);
      "bar"
    });

    assert!(!evaluated.get());
  }

  #[test]
  fn deeply_nested_elements_are_supported() {
    let html = "<div>".repeat(3000);
    assert_html_eq!(&html, &html);
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
  fn hidden_until_found_is_case_insensitive() {
    assert_html_eq!(
      "<div hidden=\"UNTIL-FOUND\"></div>",
      "<div hidden=\"until-found\"></div>"
    );
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn hidden_until_found_is_not_boolean() {
    assert_html_eq!("<div hidden=\"until-found\"></div>", "<div hidden></div>");
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
  fn inline_boundary_whitespace_is_canonicalized() {
    #[track_caller]
    fn case(actual: &str, expected: &str) {
      assert_html_eq!(actual, expected);
    }

    case("<p>foo<b> bar</b></p>", "<p>foo <b>bar</b></p>");
    case("<p><b>foo </b>bar</p>", "<p><b>foo</b> bar</p>");
    case(
      "<p>foo <script></script> bar</p>",
      "<p>foo<script></script> bar</p>",
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
  fn literal_space_markers_are_escaped() {
    assert_html_eq!("<p>foo\u{2423}<b>bar</b></p>", "<p>foo <b>bar</b></p>");
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
  fn namespace_prefixes_are_preserved() {
    assert_html_eq!(
      "<svg><a xlink:href=\"foo\"></a></svg>",
      "<svg><a href=\"foo\"></a></svg>"
    );
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
  fn part_tokens_are_case_sensitive() {
    assert_html_eq!("<div part=\"Foo\"></div>", "<div part=\"foo\"></div>");
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
  #[should_panic(expected = "assert_html_eq! failed")]
  fn preserves_xmp_whitespace() {
    assert_html_eq!("<xmp>foo  bar</xmp>", "<xmp>foo bar</xmp>");
  }

  #[test]
  fn raw_text_is_not_escaped() {
    assert_eq!(
      Normalizer::normalize(&Html::parse_document(
        "<script>foo < bar</script>"
      )),
      indoc! {"
        <html>
          <head>
            <script>
              foo < bar
            </script>
          </head>
          <body></body>
        </html>"},
    );
  }

  #[test]
  fn self_closing_non_void_normalizes() {
    assert_html_eq!("<div/>", "<div></div>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn significant_newline_before_element_is_preserved() {
    assert_html_eq!("<pre>foo\n<b>bar</b></pre>", "<pre>foo<b>bar</b></pre>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn significant_trailing_newline_is_preserved() {
    assert_html_eq!("<pre>foo\n</pre>", "<pre>foo</pre>");
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
  #[should_panic(expected = "assert_html_eq! failed")]
  fn unknown_elements_are_inline() {
    assert_html_eq!(
      "<p>foo <x-bar>baz</x-bar></p>",
      "<p>foo<x-bar>baz</x-bar></p>"
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
  #[should_panic(expected = "assert_html_eq! failed")]
  fn whitespace_around_inline_blocks_matters() {
    assert_html_eq!("<label>foo <input></label>", "<label>foo<input></label>");
  }

  #[test]
  #[should_panic(expected = "assert_html_eq! failed")]
  fn whitespace_inside_inline_elements_matters() {
    assert_html_eq!("<p>foo<b> bar</b></p>", "<p>foo<b>bar</b></p>");
  }

  #[test]
  fn whitespace_only_text_runs_between_elements_are_ignored() {
    assert_html_eq!(
      "<div>a</div>\n   \t  <div>b</div>",
      "<div>a</div><div>b</div>"
    );
  }
}
