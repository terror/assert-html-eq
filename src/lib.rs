use {
  ego_tree::NodeRef,
  scraper::{ElementRef, Node},
};

fn is_whitespace_text(node: &NodeRef<Node>) -> bool {
  matches!(node.value(), Node::Text(t) if t.trim().is_empty())
}

fn normalize_whitespace(text: &str) -> String {
  text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn compare_elements(elem1: &ElementRef, elem2: &ElementRef) {
  assert_eq!(
    elem1.value().name(),
    elem2.value().name(),
    "Tag name mismatch"
  );

  let mut attrs1 = elem1.value().attrs().collect::<Vec<_>>();
  let mut attrs2 = elem2.value().attrs().collect::<Vec<_>>();

  attrs1.sort_by_key(|a| a.0);
  attrs2.sort_by_key(|a| a.0);

  assert_eq!(
    attrs1,
    attrs2,
    "Attributes mismatch on <{}>",
    elem1.value().name()
  );

  let children1 = elem1
    .children()
    .filter(|n| !is_whitespace_text(n))
    .collect::<Vec<_>>();

  let children2 = elem2
    .children()
    .filter(|n| !is_whitespace_text(n))
    .collect::<Vec<_>>();

  assert_eq!(
    children1.len(),
    children2.len(),
    "Different number of children in <{}>",
    elem1.value().name()
  );

  for (child1, child2) in children1.iter().zip(children2.iter()) {
    match (child1.value(), child2.value()) {
      (Node::Element(_), Node::Element(_)) => {
        compare_elements(
          &ElementRef::wrap(*child1).unwrap(),
          &ElementRef::wrap(*child2).unwrap(),
        );
      }
      (Node::Text(t1), Node::Text(t2)) => {
        assert_eq!(
          normalize_whitespace(t1),
          normalize_whitespace(t2),
          "Text content mismatch in <{}>",
          elem1.value().name()
        );
      }
      _ => {
        panic!(
          "Node type mismatch in <{}>: {:?} vs {:?}",
          elem1.value().name(),
          child1.value(),
          child2.value()
        );
      }
    }
  }
}

#[doc(hidden)]
pub mod __private {
  use super::compare_elements;
  use scraper::Html;
  use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

  pub fn assert_html_eq(actual: Html, expected: Html, custom: Option<String>) {
    let result = catch_unwind(AssertUnwindSafe(|| {
      compare_elements(&actual.root_element(), &expected.root_element());
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
  #[should_panic(expected = "Tag name mismatch")]
  fn mismatch_panics() {
    crate::assert_html_eq!("<div></div>", "<span></span>");
  }
}
