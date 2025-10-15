use {
  ego_tree::NodeRef,
  scraper::{ElementRef, Html, Node},
  std::ops::Deref,
};

fn is_whitespace_text(node: &NodeRef<Node>) -> bool {
  matches!(node.value(), Node::Text(t) if t.trim().is_empty())
}

fn normalize_whitespace(text: &str) -> String {
  text.split_whitespace().collect::<Vec<_>>().join(" ")
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

fn significant_children<'a>(
  element: &ElementRef<'a>,
) -> Vec<NodeRef<'a, Node>> {
  element
    .children()
    .filter(|child| !is_whitespace_text(child))
    .collect::<Vec<_>>()
}

fn write_element(buffer: &mut String, element: &ElementRef, depth: usize) {
  let indent = "  ".repeat(depth);
  let name = element.value().name();

  buffer.push_str(&indent);
  buffer.push('<');
  buffer.push_str(name);

  let mut attrs = element.value().attrs().collect::<Vec<_>>();
  attrs.sort_by_key(|attr| attr.0);

  for (key, value) in attrs {
    buffer.push(' ');
    buffer.push_str(key);
    buffer.push_str("=\"");
    buffer.push_str(&escape_attr_value(value));
    buffer.push('"');
  }

  let children = significant_children(element);

  if children.is_empty() {
    buffer.push_str("></");
    buffer.push_str(name);
    buffer.push('>');
    buffer.push('\n');
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
        );
      }
      Node::Text(text) => {
        let normalized = normalize_whitespace(text.deref());
        if !normalized.is_empty() {
          buffer.push('\n');
          buffer.push_str(&child_indent);
          buffer.push_str(&escape_text(&normalized));
        }
      }
      Node::Comment(comment) => {
        buffer.push('\n');
        buffer.push_str(&child_indent);
        buffer.push_str("<!-- ");
        buffer.push_str(comment.deref().trim());
        buffer.push_str(" -->");
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
    }
  }

  buffer.push('\n');
  buffer.push_str(&indent);
  buffer.push_str("</");
  buffer.push_str(name);
  buffer.push('>');
  buffer.push('\n');
}

fn normalize_html(html: &Html) -> String {
  let mut buffer = String::new();
  write_element(&mut buffer, &html.root_element(), 0);
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
}
