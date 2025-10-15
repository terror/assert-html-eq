use {
  ego_tree::NodeRef,
  scraper::{ElementRef, Html, Node},
  std::{borrow::Cow, ops::Deref},
};

fn is_whitespace_significant(tag: &str) -> bool {
  matches!(tag, "code" | "pre" | "script" | "style" | "textarea")
}

fn normalize_nbsp(text: &str) -> Cow<'_, str> {
  if text.contains('\u{00A0}') {
    let mut out = String::with_capacity(text.len());

    for ch in text.chars() {
      out.push(if ch == '\u{00A0}' { ' ' } else { ch });
    }

    Cow::Owned(out)
  } else {
    Cow::Borrowed(text)
  }
}

fn normalize_text_for<'a>(stack: &[&'a str], text: &str) -> String {
  if stack.iter().any(|tag| is_whitespace_significant(tag)) {
    text.to_string()
  } else {
    normalize_nbsp(text)
      .split_whitespace()
      .collect::<Vec<_>>()
      .join(" ")
  }
}

fn should_keep_text<'a>(stack: &[&'a str], text: &str) -> bool {
  if text.trim().is_empty() {
    stack.iter().any(|tag| is_whitespace_significant(tag))
  } else {
    true
  }
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

fn escape_attr_value(input: &str) -> String {
  let mut escaped = String::with_capacity(input.len());

  for ch in input.chars() {
    match ch {
      '&' => escaped.push_str("&amp;"),
      '<' => escaped.push_str("&lt;"),
      '"' => escaped.push_str("&quot;"),
      _ => escaped.push(ch),
    }
  }

  escaped
}

fn is_boolean_attribute(name: &str) -> bool {
  matches!(
    name,
    "allowfullscreen"
      | "async"
      | "autofocus"
      | "autoplay"
      | "checked"
      | "controls"
      | "default"
      | "defer"
      | "disabled"
      | "formnovalidate"
      | "hidden"
      | "inert"
      | "ismap"
      | "itemscope"
      | "loop"
      | "multiple"
      | "muted"
      | "nomodule"
      | "novalidate"
      | "open"
      | "playsinline"
      | "readonly"
      | "required"
      | "reversed"
      | "selected"
      | "truespeed"
  )
}

struct Attr<'a> {
  name: &'a str,
  value: Option<Cow<'a, str>>,
}

fn normalize_attributes<'a>(element: &ElementRef<'a>) -> Vec<Attr<'a>> {
  let mut attrs = element
    .value()
    .attrs()
    .map(|(name, value)| {
      if name == "class" {
        let mut tokens = value.split_ascii_whitespace().collect::<Vec<_>>();
        tokens.sort_unstable();
        tokens.dedup();

        Attr {
          name,
          value: Some(Cow::Owned(tokens.join(" "))),
        }
      } else if is_boolean_attribute(name) {
        Attr { name, value: None }
      } else {
        Attr {
          name,
          value: Some(Cow::Borrowed(value)),
        }
      }
    })
    .collect::<Vec<_>>();

  attrs.sort_by(|a, b| a.name.cmp(b.name));
  attrs
}

fn write_element<'a>(
  buffer: &mut String,
  element: &ElementRef<'a>,
  depth: usize,
  stack: &mut Vec<&'a str>,
) {
  let indent = "  ".repeat(depth);
  let name = element.value().name();

  buffer.push_str(&indent);
  buffer.push('<');
  buffer.push_str(name);

  let attrs = normalize_attributes(element);

  for Attr { name: key, value } in attrs {
    buffer.push(' ');
    buffer.push_str(key);
    if let Some(value) = value {
      buffer.push_str("=\"");
      buffer.push_str(&escape_attr_value(value.as_ref()));
      buffer.push('"');
    }
  }

  stack.push(name);

  let children: Vec<NodeRef<'a, Node>> = element
    .children()
    .filter(|child| match child.value() {
      Node::Comment(_) => false,
      Node::Text(text) => should_keep_text(stack, text.deref()),
      Node::Document | Node::Fragment => false,
      _ => true,
    })
    .collect();

  if children.is_empty() {
    buffer.push_str("></");
    buffer.push_str(name);
    buffer.push('>');
    buffer.push('\n');
    stack.pop();
    return;
  }

  buffer.push('>');

  let child_indent = "  ".repeat(depth + 1);

  for child in children {
    match child.value() {
      Node::Element(_) => {
        buffer.push('\n');
        write_element(
          buffer,
          &ElementRef::wrap(child).expect("child must be an element"),
          depth + 1,
          stack,
        );
      }
      Node::Text(text) => {
        let normalized = normalize_text_for(stack, text.deref());
        if !normalized.is_empty() {
          buffer.push('\n');
          buffer.push_str(&child_indent);
          buffer.push_str(&escape_text(&normalized));
        }
      }
      Node::Doctype(doctype) => {
        buffer.push('\n');
        buffer.push_str(&child_indent);
        buffer.push_str(&format!("{doctype:?}"));
      }
      Node::ProcessingInstruction(pi) => {
        buffer.push('\n');
        buffer.push_str(&child_indent);
        buffer.push_str(&format!("{pi:?}"));
      }
      Node::Document | Node::Fragment => {}
      Node::Comment(_) => {}
    }
  }

  buffer.push('\n');
  buffer.push_str(&indent);
  buffer.push_str("</");
  buffer.push_str(name);
  buffer.push('>');
  buffer.push('\n');

  stack.pop();
}

fn normalize_html(html: &Html) -> String {
  let mut buffer = String::new();
  let mut stack = Vec::new();
  write_element(&mut buffer, &html.root_element(), 0, &mut stack);
  buffer.trim_end().to_string()
}

#[doc(hidden)]
pub mod __private {
  use {
    super::normalize_html,
    scraper::Html,
    std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
  };

  pub fn assert_html_eq(actual: Html, expected: Html, custom: Option<String>) {
    let (actual_normalized, expected_normalized) =
      (normalize_html(&actual), normalize_html(&expected));

    let result = catch_unwind(AssertUnwindSafe(|| {
      pretty_assertions::assert_eq!(expected_normalized, actual_normalized);
    }));

    match result {
      Ok(_) => {}
      Err(payload) => {
        if let Some(message) = custom {
          let detail = if let Some(msg) = payload.downcast_ref::<String>() {
            msg.clone()
          } else if let Some(msg) = payload.downcast_ref::<&str>() {
            (*msg).to_string()
          } else {
            "assert_html_eq! failed".to_string()
          };

          panic!("{message}\n{detail}");
        } else {
          resume_unwind(payload);
        }
      }
    }
  }

  pub trait IntoHtml {
    fn into_html(self) -> Html;
  }

  impl IntoHtml for Html {
    fn into_html(self) -> Html {
      self
    }
  }

  impl<'a> IntoHtml for &'a Html {
    fn into_html(self) -> Html {
      self.clone()
    }
  }

  impl<'a> IntoHtml for &'a str {
    fn into_html(self) -> Html {
      Html::parse_document(self)
    }
  }

  impl IntoHtml for String {
    fn into_html(self) -> Html {
      Html::parse_document(&self)
    }
  }

  impl<'a> IntoHtml for &'a String {
    fn into_html(self) -> Html {
      Html::parse_document(self)
    }
  }

  pub fn into_html<T: IntoHtml>(input: T) -> Html {
    input.into_html()
  }
}

#[macro_export]
macro_rules! assert_html_eq {
  ($actual:expr, $expected:expr $(,)?) => {{
    $crate::__private::assert_html_eq(
      $crate::__private::into_html($actual),
      $crate::__private::into_html($expected),
      None
    );
  }};
  ($actual:expr, $expected:expr, $($arg:tt)+) => {{
    $crate::__private::assert_html_eq(
      $crate::__private::into_html($actual),
      $crate::__private::into_html($expected),
      Some(format!($($arg)+)),
    );
  }};
}

#[cfg(test)]
mod tests {
  use scraper::Html;

  #[test]
  fn compares_html_strings() {
    crate::assert_html_eq!(
      "<div><span>Hello</span></div>",
      "<div>
        <span>
          Hello
        </span>
      </div>",
    );
  }

  #[test]
  fn accepts_html_values() {
    crate::assert_html_eq!(
      Html::parse_document("<p>Text</p>"),
      Html::parse_document("<p>Text</p>")
    );
  }

  #[test]
  #[should_panic]
  fn mismatch_panics() {
    crate::assert_html_eq!("<div></div>", "<span></span>");
  }

  #[test]
  fn ignores_comments() {
    crate::assert_html_eq!(
      "<div><!-- noisy --><span>Text</span></div>",
      "<div><span>Text</span></div>"
    );
  }

  #[test]
  fn normalizes_class_token_order() {
    crate::assert_html_eq!(
      "<div class=\"b a c b\"></div>",
      "<div class=\"a b c\"></div>"
    );
  }

  #[test]
  fn normalizes_boolean_attributes() {
    crate::assert_html_eq!(
      "<input type=\"checkbox\" disabled=\"disabled\" checked=\"\" />",
      "<input type=\"checkbox\" disabled checked>"
    );
  }

  #[test]
  fn normalizes_nbsp_as_space() {
    crate::assert_html_eq!("<p>A&nbsp;B</p>", "<p>A B</p>");
  }

  #[test]
  #[should_panic]
  fn preserves_pre_whitespace() {
    crate::assert_html_eq!(
      "<pre>line  with  spaces</pre>",
      "<pre>line with spaces</pre>"
    );
  }

  #[test]
  #[should_panic]
  fn preserves_script_whitespace() {
    crate::assert_html_eq!(
      "<script>if (x) {\n  doThing();\n}</script>",
      "<script>if (x) { doThing(); }</script>"
    );
  }
}
