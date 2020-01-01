use std::borrow::Cow;
use std::fmt::{self as stdfmt, Debug};
use std::ops::Deref;

/// An owned or borrowed byte array.
///
/// `Barr` provides a thin wrapper around [`Cow<u8>`]. The smart pointer allows the byte array to
/// be borrowed up until the point of mutation, at which point a clone is made.
///
/// [`Cow<u8>`]: std::borrow::Cow
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Barr<'a> {
    /// Inner access to the underlying clone-on-write smart pointer.
    pub inner: Cow<'a, [u8]>,
}

impl<'a> Barr<'a> {
    /// A new _borrowed_ byte array. `Barr` has the same lifetime as the borrowed data.
    pub const fn brwed(bytes: &'a [u8]) -> Self {
        Self {
            inner: Cow::Borrowed(bytes),
        }
    }

    /// A new _owned_ byte array. Takes ownership of the vector of bytes and has a `'static`
    /// lifetime.
    pub const fn owned(bytes: Vec<u8>) -> Barr<'static> {
        Barr {
            inner: Cow::Owned(bytes),
        }
    }

    /// Represent the byte array as a slice.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let barr = Barr::owned(vec![0,1,2]);
    /// assert_eq!(barr.as_bytes(), &[0,1,2]);
    /// ```
    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    /// Acquires a mutable reference to the owned vector of bytes.
    ///
    /// **_Allocates a new vector and clones the data if not already owned._**
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut barr = Barr::brwed([0,1,2,3].as_ref());
    /// barr.to_mut().push(4);
    /// assert_eq!(barr, Barr::brwed([0,1,2,3,4].as_ref()));
    /// ```
    pub fn to_mut(&mut self) -> &mut Vec<u8> {
        self.inner.to_mut()
    }

    /// Makes the underlying byte array owned.
    ///
    /// **_Allocates a new vector and clones the data if it is not already owned_**.
    /// This upgrades the lifetime to `'static`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// let mut barr = Barr::brwed([0,1,2,3].as_ref());    
    /// assert_eq!(barr.to_owned(), Barr::owned(vec![0,1,2,3]));
    ///	```
    pub fn to_owned(self) -> Barr<'static> {
        Barr::owned(self.inner.into_owned())
    }
}

impl<'a> Deref for Barr<'a> {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.inner
    }
}

impl<'a> AsRef<[u8]> for Barr<'a> {
    fn as_ref(&self) -> &[u8] {
        &*self
    }
}

impl<'a> From<&'a [u8]> for Barr<'a> {
    fn from(bytes: &'a [u8]) -> Self {
        Self::brwed(bytes)
    }
}

impl From<Vec<u8>> for Barr<'static> {
    fn from(bytes: Vec<u8>) -> Self {
        Self::owned(bytes)
    }
}

impl<'a> Debug for Barr<'a> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        write!(f, "{:?}", &self.inner)
    }
}

impl<'a> PartialEq<[u8]> for Barr<'a> {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_ref() == other
    }
}

impl<'a> PartialEq<[u8]> for &Barr<'a> {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_ref() == other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq() {
        let one: Barr = b"Hello, world".to_vec().into();
        let two: Barr = b"Hello, world"[..].into();
        assert_eq!(one, two);
    }

    #[test]
    fn test_hash() {
        let mut set = std::collections::HashSet::new();
        set.insert(Barr::owned(b"Hello, world!".to_vec()));
        let two = Barr::brwed(b"Hello, world!");

        assert_eq!(set.contains(&two), true);
    }

    #[test]
    fn test_to_mut() {
        let mut x: Barr = b"Hello"[..].into();
        assert_eq!(&x, &b"Hello"[..]);
        x.to_mut().extend(b", world!".iter());
        assert_eq!(&x, b"Hello, world!"[..]);
    }

    #[test]
    fn test_as_ref() {
        let x = Barr::brwed(b"Hello, world!");
        assert_eq!(x.as_ref(), &b"Hello, world!"[..]);
    }

    #[test]
    fn fmt_test() {
        let x = Barr::brwed(&[100]);
        assert_eq!(&format!("{:?}", x), "[100]");
        let x = Barr::owned(vec![100]);
        assert_eq!(&format!("{:?}", x), "[100]");
    }
}
