//! Navigate around a [`Kserd`].
//!
//! # Reading
//! It is cumbersome when trying to loop through a nested `Kserd` structure. There is a [`Navigator`]
//! structure that can be used that flattens the tree structure when constructed to make use of
//! indexing and iterators throughout the `Kserd`.
//!
//! [`Kserd`]: crate::Kserd
//! [`Navigator`]: crate::nav::Navigator
// TODO: Lifetimes should be scoped to the borrowed kserd as much as possible.
// TODO: Can probably remove the 'node lifetime, as it is borrowed from the Navigator
use super::*;

mod navigator;
mod node;
mod value;

/// A structure to efficiently navigate through a [`Kserd`].
///
/// When trying to interrogate a `Kserd` it can become cumbersome to loop through nested children.
/// The `Navigator` provides a structure around a root `Kserd` which then allows for efficient
/// looping through the structure.
///
/// When a `Navigator` is constructed it flattens the tree structure into a depth-first vector,
/// wrapping each `Kserd` into a [`Node`] which contains navigational information such are parent
/// or children.
///
/// [`Kserd`]: crate::Kserd
/// [`Node`]: crate::nav::Node
pub struct Navigator<'a, 'k> {
    inner: Vec<NodeInner<'a, 'k>>,
}

/// A node in the [`Kserd`] tree.
///
/// A `Node` wraps a pointer to the `Kserd` value, and carries positional information about itself
/// and surrounding family. As such `Node`s can walk the tree structures, and have uses when
/// interrogating a `Kserd` data structure.
///
/// Since a `Node` carries a reference to the `Navigator` and the `Kserd`, it is a read-only
/// structure and no mutation can occur.
///
/// [`Kserd`]: crate::Kserd
#[derive(Copy, Clone)]
pub struct Node<'a, 'k, 'nav, 'node> {
    nav: &'nav Navigator<'a, 'k>,
    node: &'node NodeInner<'a, 'k>,
}

struct NodeInner<'a, 'k> {
    kserd: &'a Kserd<'k>,
    idx: usize,
    parent: Option<usize>,
    value_idx_in_parent: Option<usize>,
    key_idx_in_parent: Option<usize>,
    value_children: Vec<usize>,
    key_children: Vec<usize>,
}

/// The node's value type.
///
/// Differs to [`Value`] in that it groups primitives, and provides
/// iterators over nested `Kserd`s that themselves return `Node`s, such that position information
/// is retained if walking through a `Kserd`.
///
/// [`Value`]: crate::Value
pub enum NodeValue<'a, 'k, 'nav, 'node> {
    /// A primitive value.
    Primitive,
    /// A tuple value.
    Tuple(SeqIter<'a, 'k, 'nav, 'node>),
    /// A container value.
    Cntr(NamedIter<'a, 'k, 'nav, 'node>),
    /// A sequence value.
    Seq(SeqIter<'a, 'k, 'nav, 'node>),
    /// A map value.
    Map(KeyedIter<'a, 'k, 'nav, 'node>),
}

/// Iterator of nodes.
pub struct NodeIter<'a, 'k, 'nav> {
    iter: std::slice::Iter<'nav, NodeInner<'a, 'k>>,
    nav: &'nav Navigator<'a, 'k>,
}

/// Iterator for sequential items like sequences.
/// Can be used for keys of maps as well.
pub struct SeqIter<'a, 'k, 'nav, 'node> {
    seq: std::slice::Iter<'node, usize>,
    nav: &'nav Navigator<'a, 'k>,
}

/// Iterator for named items like in containers.
pub struct NamedIter<'a, 'k, 'nav, 'node> {
    names: std::collections::btree_map::Keys<'node, Kstr<'k>, Kserd<'k>>,
    values: std::slice::Iter<'node, usize>,
    nav: &'nav Navigator<'a, 'k>,
}

/// Iterator for keyed items like in maps.
pub struct KeyedIter<'a, 'k, 'nav, 'node> {
    keys: std::slice::Iter<'node, usize>,
    values: std::slice::Iter<'node, usize>,
    nav: &'nav Navigator<'a, 'k>,
}
