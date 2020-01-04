use std::borrow::Cow;
use std::fmt::{self as stdfmt, Debug, Display};
use std::ops::Deref;

/// An owned or borrowed string.
///
/// `Kstr` provides a thin wrapper around [`Cow<str>`]. The smart pointer allows the string to be
/// borrowed up until the point of mutation, at which point a clonse is made.
///
/// [`Cow<str>`]: std::borrow::Cow
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Kstr<'a> {
    /// Inner access to the underlying clone-on-write smart pointer.
    pub inner: Cow<'a, str>,
}

impl<'a> Kstr<'a> {
    /// A new _borrowed_ string. `Kstr` has the same lifetime as the borrowed string.
    pub const fn brwed(s: &'a str) -> Self {
        Self {
            inner: Cow::Borrowed(s),
        }
    }

    /// A new _owned_ string. Takes ownership of the string and has a `'static` lifetime.
    pub const fn owned(s: String) -> Kstr<'static> {
        Kstr {
            inner: Cow::Owned(s),
        }
    }

    /// Represent the string as a string slice.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kstr = Kstr::owned(String::from("Hello, world!"));
    /// assert_eq!(kstr.as_str(), "Hello, world!");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Acquires a mutable reference to the owned string.
    ///
    /// **_Clones the string if it not already owned._**
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut kstr = Kstr::brwed("foo");
    /// kstr.to_mut().make_ascii_uppercase();
    /// assert_eq!(kstr, Kstr::brwed("FOO"));
    /// ```
    pub fn to_mut(&mut self) -> &mut String {
        self.inner.to_mut()
    }

    /// Makes the underlying string owned.
    ///
    /// **_Clones the string if it is not already owned_**.
    /// This upgrades the lifetime to `'static`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let kstr = Kstr::brwed("Hello, world!");
    /// assert_eq!(kstr.to_owned(), Kstr::owned(String::from("Hello, world!")));
    ///	```
    pub fn to_owned(self) -> Kstr<'static> {
        Kstr::owned(self.inner.into_owned())
    }
}

impl<'a> Deref for Kstr<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        &self.inner
    }
}

impl<'a> AsRef<str> for Kstr<'a> {
    fn as_ref(&self) -> &str {
        &*self
    }
}

impl<'a> From<&'a str> for Kstr<'a> {
    fn from(s: &'a str) -> Self {
        Self::brwed(s)
    }
}

impl From<String> for Kstr<'static> {
    fn from(s: String) -> Self {
        Self::owned(s)
    }
}

impl<'a> Display for Kstr<'a> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl<'a> Debug for Kstr<'a> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        write!(f, "{:?}", &self.inner)
    }
}

impl<'a> PartialEq<str> for Kstr<'a> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq() {
        let one: Kstr = String::from("Hello, world").into();
        let two: Kstr = "Hello, world".into();
        assert_eq!(one, two);
    }

    #[test]
    fn test_hash() {
        let mut set = std::collections::HashSet::new();
        set.insert(Kstr::owned("Hello, world!".to_owned()));
        let two = Kstr::brwed("Hello, world!");

        assert_eq!(set.contains(&two), true);
    }

    #[test]
    fn to_mut_test() {
        let mut kstr = Kstr::brwed("Hello");
        assert_eq!(&kstr, "Hello");
        kstr.to_mut().push_str(", world!");
        assert_eq!(&kstr, "Hello, world!");
    }

    #[test]
    fn as_ref_test() {
        let kstr = Kstr::brwed("Hello, world!");
        assert_eq!(kstr.as_ref(), "Hello, world!");
    }

    #[test]
    fn display_test() {
        let kstr = Kstr::brwed("Hello, world!");
        assert_eq!(&kstr.to_string(), "Hello, world!");
    }
}
