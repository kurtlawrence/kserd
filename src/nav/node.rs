use super::*;
use std::collections::VecDeque;
use std::fmt;

impl<'a, 'k, 'nav: 'node, 'node> Node<'a, 'k, 'nav, 'node> {
    /// The associated [`Navigator`] where the `Node` comes from.
    ///
    /// [`Navigator`]: super::Navigator
    pub fn nav(&self) -> &Navigator<'a, 'k> {
        self.nav
    }

    /// The index in the [`Navigator`] of this node.
    ///
    /// The `Navigator` flattens the tree structure into a depth-first vector, and as such each
    /// node can be referenced by indices.
    ///
    /// [`Navigator`]: super::Navigator
    pub fn index(&self) -> usize {
        self.node.idx
    }

    /// The parent of this node, unless it is the root node.
    ///
    /// Only the root does not have a parent. Nested structures are parents to the `Kserd`s they
    /// capture. This includes keys of maps.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    ///
    /// let root = nav.root();
    ///
    /// // root will not have parent
    /// assert_eq!(root.parent(), None);
    ///
    /// let key = nav.get(1).unwrap();
    /// let value = nav.get(2).unwrap();
    ///
    /// // key and value have root at parent
    /// assert_eq!(key.parent(), Some(root));
    /// assert_eq!(value.parent(), Some(root));
    /// ```
    pub fn parent(&self) -> Option<Node<'a, 'k, 'nav, 'node>> {
        self.node.parent.and_then(|p| self.nav.get(p))
    }

    /// The node is a key in a map.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    ///
    /// let key = nav.get(1).unwrap();
    /// let value = nav.get(2).unwrap();
    ///
    /// assert_eq!(key.is_key(), true);
    /// assert_eq!(value.is_key(), false);
    /// ```
    pub fn is_key(&self) -> bool {
        self.node.key_idx_in_parent.is_some()
    }

    /// Returns the index that this node sits at inside the parent data structure.
    ///
    /// Each `Kserd`, expecting the root, sits inside some ordered structure (sequences and tuples
    /// are vectors, containers and maps are _ordered_ B-Tree maps). Key-value pairs in maps share
    /// the same index in the parent.
    ///
    /// Roots have an index of `0` as default, rather than returning `Some(usize)`. You can't
    /// contextually use the index anyway.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    ///     (Kserd::new_num(3.14), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    ///
    /// let root = nav.root();
    ///
    /// // root will parent index of 0
    /// assert_eq!(root.idx_in_parent(), 0);
    ///
    /// let key1 = nav.get(1).unwrap();
    /// let value1 = nav.get(2).unwrap();
    ///
    /// // key and value have same index
    /// assert_eq!(key1.idx_in_parent(), 0);
    /// assert_eq!(value1.idx_in_parent(), 0);
    ///
    /// let key2 = nav.get(3).unwrap();
    /// let value2 = nav.get(4).unwrap();
    ///
    /// // key and value have same index
    /// assert_eq!(key2.idx_in_parent(), 1);
    /// assert_eq!(value2.idx_in_parent(), 1);
    /// ```
    pub fn idx_in_parent(&self) -> usize {
        self.node
            .key_idx_in_parent
            .or(self.node.value_idx_in_parent)
            .unwrap_or(0)
    }

    /// The underlying [`Kserd`](crate::Kserd).
    pub fn kserd(&self) -> &'a Kserd<'k> {
        self.node.kserd
    }

    /// The node-type value.
    pub fn value(&self) -> NodeValue<'a, 'k, 'nav, 'node> {
        self.node.value(self.nav)
    }

    /// The children, if it is a map, the values.
    ///
    /// If the node is primitive, an empty iterator is returned.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    ///     (Kserd::new_num(3.14), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    /// let root = nav.root();
    /// let mut vals = root.value_children().map(|x| x.index());
    ///
    /// assert_eq!(vals.next(), Some(2));
    /// assert_eq!(vals.next(), Some(4));
    /// assert_eq!(vals.next(), None);
    /// ```
    pub fn value_children(&self) -> SeqIter {
        SeqIter {
            seq: self.node.value_children.iter(),
            nav: self.nav,
        }
    }

    /// If the node is a map, returns the keys, otherwise an empty iterator is returned.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    ///     (Kserd::new_num(3.14), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    /// let root = nav.root();
    /// let mut vals = root.key_children().map(|x| x.index());
    ///
    /// assert_eq!(vals.next(), Some(1));
    /// assert_eq!(vals.next(), Some(3));
    /// assert_eq!(vals.next(), None);
    /// ```
    pub fn key_children(&self) -> SeqIter {
        SeqIter {
            seq: self.node.key_children.iter(),
            nav: self.nav,
        }
    }

    /// From this node return the descendants depth-first.
    /// _Does not return `self` in collection._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    ///     (Kserd::new_num(3.14), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    /// let depth = nav.root().descendants_depth();
    /// let mut vals = depth.iter().map(|x| x.index());
    ///
    /// assert_eq!(vals.next(), Some(1));
    /// assert_eq!(vals.next(), Some(2));
    /// assert_eq!(vals.next(), Some(3));
    /// assert_eq!(vals.next(), Some(4));
    /// assert_eq!(vals.next(), None);
    /// ```
    pub fn descendants_depth(&self) -> Vec<Self> {
        // struggle to use iterators as I need to switch between key and value,
        // but key may be empty if not a map =(
        fn push_children(node: &NodeInner, stack: &mut Vec<usize>) {
            use std::cmp::max;

            let rng = (0..max(node.key_children.len(), node.value_children.len())).rev();

            for i in rng {
                // we do value first, as when popping off, key will be first to pop
                if let Some(x) = node.value_children.get(i) {
                    stack.push(*x);
                }

                if let Some(x) = node.key_children.get(i) {
                    stack.push(*x);
                }
            }
        }

        let mut todo =
            Vec::with_capacity(self.node.key_children.len() + self.node.value_children.len());

        push_children(self.node, &mut todo);

        let mut nodes = Vec::new();

        while let Some(i) = todo.pop() {
            nodes.push(self.nav.get(i).unwrap());
            let n = self.nav.inner.get(i).unwrap();
            push_children(n, &mut todo);
        }

        nodes
    }

    /// From this node return the descendants breadth-first.
    /// _Does not return `self` in collection._
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_unit(), Kserd::new_num(0)),
    ///     (Kserd::new_num(3.14), Kserd::new_num(0)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    /// let breadth = nav.root().descendants_breadth();
    /// let mut vals = breadth.iter().map(|x| x.index());
    ///
    /// assert_eq!(vals.next(), Some(1));
    /// assert_eq!(vals.next(), Some(3)); // keys first
    /// assert_eq!(vals.next(), Some(2));
    /// assert_eq!(vals.next(), Some(4));
    /// assert_eq!(vals.next(), None);
    /// ```    
    pub fn descendants_breadth(&self) -> Vec<Self> {
        fn children<'a>(node: &'a NodeInner) -> impl Iterator<Item = usize> + 'a {
            node.key_children
                .iter()
                .chain(node.value_children.iter())
                .cloned()
        }

        let mut todo = children(self.node).collect::<VecDeque<_>>();

        let mut nodes = Vec::new();

        while let Some(i) = todo.pop_front() {
            nodes.push(self.nav.get(i).unwrap());
            let n = self.nav.inner.get(i).unwrap();
            todo.extend(children(n));
        }

        nodes
    }
}

impl<'a, 'k, 'node, 'nav> fmt::Debug for Node<'a, 'k, 'node, 'nav> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut dbg = f.debug_struct("Node");

        dbg.field("id", &self.kserd().id());

        match self.value() {
            NodeValue::Primitive => {
                dbg.field("value-type", &"Primitive");
                dbg.field("value", &self.kserd().val)
            }
            NodeValue::Tuple(_) => dbg.field("value-type", &"Tuple"),
            NodeValue::Cntr(_) => dbg.field("value-type", &"Cntr"),
            NodeValue::Seq(_) => dbg.field("value-type", &"Seq"),
            NodeValue::Map(_) => dbg.field("value-type", &"Map"),
        };

        dbg.field("index", &self.index())
            .field("parent", &self.parent().map(|x| x.index()))
            .field("is_key", &self.is_key())
            .field("idx_in_parent", &self.idx_in_parent())
            .finish()
    }
}

impl<'a, 'k, 'node, 'nav> PartialEq for Node<'a, 'k, 'node, 'nav> {
    fn eq(&self, rhs: &Node) -> bool {
        use std::ptr::eq;
        eq(self.node, rhs.node) && eq(self.nav, rhs.nav)
    }
}

impl<'a, 'k, 'node, 'nav> Eq for Node<'a, 'k, 'node, 'nav> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fmt_debug_test() {
        let kserd = Kserd::new_unit();
        let nav = Navigator::new(&kserd);
        let s = format!("{:#?}", nav.root());
        println!("{}", s);
        assert_eq!(
            &s,
            r#"Node {
    id: None,
    value-type: "Primitive",
    value: Unit,
    index: 0,
    parent: None,
    is_key: false,
    idx_in_parent: 0,
}"#
        );

        let kserd = Kserd::new(Value::Tuple(vec![]));
        let nav = Navigator::new(&kserd);
        let s = format!("{:#?}", nav.root());
        println!("{}", s);
        assert_eq!(
            &s,
            r#"Node {
    id: None,
    value-type: "Tuple",
    index: 0,
    parent: None,
    is_key: false,
    idx_in_parent: 0,
}"#
        );

        let kserd = Kserd::new_cntr::<_, String>(vec![]).unwrap();
        let nav = Navigator::new(&kserd);
        let s = format!("{:#?}", nav.root());
        println!("{}", s);
        assert_eq!(
            &s,
            r#"Node {
    id: None,
    value-type: "Cntr",
    index: 0,
    parent: None,
    is_key: false,
    idx_in_parent: 0,
}"#
        );

        let kserd = Kserd::new(Value::Seq(vec![]));
        let nav = Navigator::new(&kserd);
        let s = format!("{:#?}", nav.root());
        println!("{}", s);
        assert_eq!(
            &s,
            r#"Node {
    id: None,
    value-type: "Seq",
    index: 0,
    parent: None,
    is_key: false,
    idx_in_parent: 0,
}"#
        );

        let kserd = Kserd::new_map(vec![]);
        let nav = Navigator::new(&kserd);
        let s = format!("{:#?}", nav.root());
        println!("{}", s);
        assert_eq!(
            &s,
            r#"Node {
    id: None,
    value-type: "Map",
    index: 0,
    parent: None,
    is_key: false,
    idx_in_parent: 0,
}"#
        );
    }

    #[test]
    fn node_eq() {
        let kserd = Kserd::new_unit();
        let nav1 = Navigator::new(&kserd);
        let root1 = nav1.root();
        let root2 = nav1.root();
        assert_eq!(root1, root2);
        let nav2 = Navigator::new(&kserd);
        let root3 = nav2.root();
        assert_ne!(root1, root3);
    }
}
