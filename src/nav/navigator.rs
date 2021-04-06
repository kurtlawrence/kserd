use super::*;
use std::collections::HashMap; // TODO, switch to fxhash for increased speed

impl<'a, 'k> Navigator<'a, 'k> {
    /// Construct a new `Navigator`.
    ///
    /// `Navigator` borrows a [`Kserd`] as a root. It flattens the structure into a vector using a
    /// depth-first approach, and creates navigational links between `Kserd` nodes like parent and children.
    ///
    /// [`Kserd`]: crate::Kserd
    pub fn new(root: &'a Kserd<'k>) -> Self {
        #[derive(Default)]
        struct Info {
            parent: Option<usize>,
            value_idx_in_parent: Option<usize>,
            key_idx_in_parent: Option<usize>,
        }

        let mut stack = vec![(Info::default(), root)];

        let mut inner = Vec::new();

        // first populate parent links

        while let Some((info, node)) = stack.pop() {
            let len = inner.len();

            let Info {
                parent,
                value_idx_in_parent,
                key_idx_in_parent,
            } = info;

            inner.push(NodeInner {
                kserd: node,
                idx: len,
                parent,
                value_idx_in_parent,
                key_idx_in_parent,
                value_children: Vec::new(),
                key_children: Vec::new(),
            });

            let parent = Some(len);

            match &node.val {
                Value::Tuple(v) => {
                    for i in (0..v.len()).rev() {
                        stack.push((
                            Info {
                                parent,
                                value_idx_in_parent: Some(i),
                                ..Info::default()
                            },
                            &v[i],
                        ))
                    }
                }
                Value::Cntr(v) => {
                    for (i, n) in v.values().enumerate().rev() {
                        stack.push((
                            Info {
                                parent,
                                value_idx_in_parent: Some(i),
                                ..Info::default()
                            },
                            n,
                        ));
                    }
                }
                Value::Seq(v) => {
                    for i in (0..v.len()).rev() {
                        stack.push((
                            Info {
                                parent,
                                value_idx_in_parent: Some(i),
                                ..Info::default()
                            },
                            &v[i],
                        ));
                    }
                }
                Value::Map(v) => {
                    for (i, kvp) in v.iter().enumerate().rev() {
                        // push the value first (as it's reversed.)
                        stack.push((
                            Info {
                                parent,
                                value_idx_in_parent: Some(i),
                                ..Info::default()
                            },
                            kvp.1,
                        ));
                        stack.push((
                            Info {
                                parent,
                                key_idx_in_parent: Some(i),
                                ..Info::default()
                            },
                            kvp.0,
                        ));
                    }
                }
                _ => (),
            }
        }

        // second, link the children to the parent
        struct Info2 {
            idx: usize,
            value_idx_in_parent: Option<usize>,
            key_idx_in_parent: Option<usize>,
        }

        let mut parent_map = HashMap::with_capacity(inner.len());

        for child in inner.iter() {
            if let Some(parent) = child.parent {
                parent_map
                    .entry(parent)
                    .or_insert_with(Vec::new)
                    .push(Info2 {
                        idx: child.idx,
                        value_idx_in_parent: child.value_idx_in_parent,
                        key_idx_in_parent: child.key_idx_in_parent,
                    });
            }
        }

        for (parent, info_vec) in parent_map {
            let mut value_children = info_vec
                .iter()
                .filter_map(|x| x.value_idx_in_parent.map(|y| (y, x.idx)))
                .collect::<Vec<(usize, usize)>>();

            value_children.sort_by_key(|x| x.0);

            inner
                .get_mut(parent)
                .unwrap()
                .value_children
                .extend(value_children.into_iter().map(|x| x.1));

            // repeat for keys

            let mut key_children = info_vec
                .iter()
                .filter_map(|x| x.key_idx_in_parent.map(|y| (y, x.idx)))
                .collect::<Vec<(usize, usize)>>();

            key_children.sort_by_key(|x| x.0);

            inner
                .get_mut(parent)
                .unwrap()
                .key_children
                .extend(key_children.into_iter().map(|x| x.1));
        }

        let ret = Self { inner };

        debug_assert!(
            validate(&ret).unwrap_or(false),
            "navigator construction invalid"
        );

        ret
    }

    /// The root node, that same as the `Kserd` which was seeded as the root.
    ///
    /// Same as index `0`.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_num(3.14);
    /// let nav = Navigator::new(&kserd);
    ///
    /// assert_eq!(nav.root().index(), 0);
    /// assert_eq!(nav.root().kserd(), &kserd);
    /// ```
    pub fn root<'nav: 'node, 'node>(&'nav self) -> Node<'a, 'k, 'nav, 'node> {
        let node = self.inner.first().unwrap();

        Node { node, nav: self }
    }

    /// Get a node at an index.
    ///
    /// The `Navigator` flattens the tree structure into a vector using a depth-first approach. Use
    /// the index to quickly obtain a node.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_num(1), Kserd::new_num(2)),
    ///     (Kserd::new_num(3), Kserd::new_num(4)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    ///
    /// let get = |idx| nav.get(idx).map(|x| x.kserd());
    ///
    /// assert_eq!(get(1), Some(&Kserd::new_num(1)));
    /// assert_eq!(get(2), Some(&Kserd::new_num(2)));
    /// assert_eq!(get(3), Some(&Kserd::new_num(3)));
    /// assert_eq!(get(4), Some(&Kserd::new_num(4)));
    /// ```
    pub fn get<'nav: 'node, 'node>(&'nav self, index: usize) -> Option<Node<'a, 'k, 'nav, 'node>> {
        self.inner.get(index).map(|node| Node { node, nav: self })
    }

    /// The number of nodes.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// The navigator has no nodes.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Iterate over the nodes in depth-first order.
    ///
    /// # Example
    /// ```rust
    /// # use kserd::*;
    /// use kserd::nav::*;
    ///
    /// let kserd = Kserd::new_map(vec![
    ///     (Kserd::new_num(1), Kserd::new_num(2)),
    ///     (Kserd::new_num(3), Kserd::new_num(4)),
    /// ]);
    ///
    /// let nav = Navigator::new(&kserd);
    ///
    /// let mut nodes = nav.nodes();
    ///
    /// let mut next = || nodes.next().map(|x| x.kserd());
    ///
    /// next(); // skip the root (hard to equality check)
    ///
    /// assert_eq!(next(), Some(&Kserd::new_num(1)));
    /// assert_eq!(next(), Some(&Kserd::new_num(2)));
    /// assert_eq!(next(), Some(&Kserd::new_num(3)));
    /// assert_eq!(next(), Some(&Kserd::new_num(4)));
    /// ```
    pub fn nodes<'nav>(&'nav self) -> NodeIter<'a, 'k, 'nav> {
        NodeIter {
            iter: self.inner.iter(),
            nav: self,
        }
    }
}

impl<'a, 'k, 'nav> Iterator for NodeIter<'a, 'k, 'nav> {
    type Item = Node<'a, 'k, 'nav, 'nav>;

    fn next(&mut self) -> Option<Node<'a, 'k, 'nav, 'nav>> {
        self.iter.next().map(|node| Node {
            node,
            nav: self.nav,
        })
    }
}

#[cfg(not(debug_assertions))]
fn validate(_: &Navigator) -> Option<bool> {
    Some(true)
}

#[cfg(debug_assertions)]
#[allow(clippy::all)]
fn validate(navigator: &Navigator) -> Option<bool> {
    let v = &navigator.inner;

    if v.first()?.parent.is_some() {
        panic!("first element should be root");
    }

    if v.iter().filter(|x| x.parent.is_none()).count() != 1 {
        panic!("only one root should exist");
    }

    for i in 0..v.len() {
        let node = v.get(i)?;
        if node.idx != i {
            panic!("node index should match its index in the vector");
        }
    }

    // check index in parent matches
    for node in v {
        if let Some(parent) = node.parent {
            let parent = v.get(parent)?;

            if let Some(idx) = node.value_idx_in_parent {
                match &parent.kserd.val {
                    Value::Tuple(v) => assert_eq!(v.get(idx)?, node.kserd),
                    Value::Seq(v) => assert_eq!(v.get(idx)?, node.kserd),
                    Value::Cntr(v) => assert_eq!(v.values().nth(idx)?, node.kserd),
                    Value::Map(v) => assert_eq!(v.values().nth(idx)?, node.kserd),
                    _ => (),
                }
            }

            if let Some(idx) = node.key_idx_in_parent {
                if let Value::Map(v) = &parent.kserd.val {
                    assert_eq!(v.keys().nth(idx)?, node.kserd);
                }
            }

            match &parent.kserd.val {
                Value::Tuple(_) | Value::Seq(_) | Value::Cntr(_) | Value::Map(_) => (),
                _ => assert!(node.value_idx_in_parent.is_none()),
            }

            match &parent.kserd.val {
                Value::Map(_) => (),
                _ => assert!(node.key_idx_in_parent.is_none()),
            }
        }
    }

    // check children
    for node in v {
        match &node.kserd.val {
            Value::Tuple(l) => {
                assert_eq!(l.len(), node.value_children.len());
                assert_eq!(0, node.key_children.len());
                for i in 0..l.len() {
                    assert_eq!(v.get(*node.value_children.get(i)?)?.kserd, l.get(i)?);
                }
            }
            Value::Seq(l) => {
                assert_eq!(l.len(), node.value_children.len());
                assert_eq!(0, node.key_children.len());
                for i in 0..l.len() {
                    assert_eq!(v.get(*node.value_children.get(i)?)?.kserd, l.get(i)?);
                }
            }
            Value::Cntr(l) => {
                assert_eq!(l.len(), node.value_children.len());
                assert_eq!(0, node.key_children.len());
                for i in 0..l.len() {
                    assert_eq!(
                        v.get(*node.value_children.get(i)?)?.kserd,
                        l.values().nth(i)?
                    );
                }
            }
            Value::Map(l) => {
                assert_eq!(l.len(), node.value_children.len());
                assert_eq!(l.len(), node.key_children.len());
                for i in 0..l.len() {
                    assert_eq!(v.get(*node.key_children.get(i)?)?.kserd, l.keys().nth(i)?);
                    assert_eq!(
                        v.get(*node.value_children.get(i)?)?.kserd,
                        l.values().nth(i)?
                    );
                }
            }
            _ => {
                assert_eq!(0, node.value_children.len());
                assert_eq!(0, node.key_children.len());
            }
        }
    }

    Some(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_navigator_linear_order_tuple() {
        // going to gaurantee that navigator.nodes() comes in depth-first order of nested structure
        let kserd = Kserd::with_id(
            "0",
            Value::Tuple(vec![
                Kserd::with_id(
                    "1",
                    Value::Tuple(vec![Kserd::with_id("2", Value::Unit).unwrap()]),
                )
                .unwrap(),
                Kserd::with_id("3", Value::Unit).unwrap(),
                Kserd::with_id(
                    "4",
                    Value::Tuple(vec![Kserd::with_id("5", Value::Unit).unwrap()]),
                )
                .unwrap(),
            ]),
        )
        .unwrap();

        let nav = Navigator::new(&kserd);

        assert_eq!(
            nav.inner
                .iter()
                .map(|x| x.kserd.id().unwrap())
                .collect::<Vec<_>>(),
            &["0", "1", "2", "3", "4", "5"]
        );
    }

    #[test]
    fn check_navigator_linear_order_cntr() {
        // going to gaurantee that navigator.nodes() comes in depth-first order of nested structure
        let kserd = Kserd::with_id(
            "0",
            Value::new_cntr(vec![
                (
                    "a",
                    Kserd::with_id(
                        "1",
                        Value::new_cntr(vec![("a", Kserd::with_id("2", Value::Unit).unwrap())])
                            .unwrap(),
                    )
                    .unwrap(),
                ),
                ("b", Kserd::with_id("3", Value::Unit).unwrap()),
                (
                    "c",
                    Kserd::with_id(
                        "4",
                        Value::new_cntr(vec![("a", Kserd::with_id("5", Value::Unit).unwrap())])
                            .unwrap(),
                    )
                    .unwrap(),
                ),
            ])
            .unwrap(),
        )
        .unwrap();

        let nav = Navigator::new(&kserd);

        assert_eq!(
            nav.inner
                .iter()
                .map(|x| x.kserd.id().unwrap())
                .collect::<Vec<_>>(),
            &["0", "1", "2", "3", "4", "5"]
        );
    }

    #[test]
    fn check_navigator_linear_order_seq() {
        // going to gaurantee that navigator.nodes() comes in depth-first order of nested structure
        let kserd = Kserd::with_id(
            "0",
            Value::Seq(vec![
                Kserd::with_id(
                    "1",
                    Value::Seq(vec![Kserd::with_id("2", Value::Unit).unwrap()]),
                )
                .unwrap(),
                Kserd::with_id("3", Value::Unit).unwrap(),
                Kserd::with_id(
                    "4",
                    Value::Seq(vec![Kserd::with_id("5", Value::Unit).unwrap()]),
                )
                .unwrap(),
            ]),
        )
        .unwrap();

        let nav = Navigator::new(&kserd);

        assert_eq!(
            nav.inner
                .iter()
                .map(|x| x.kserd.id().unwrap())
                .collect::<Vec<_>>(),
            &["0", "1", "2", "3", "4", "5"]
        );
    }

    #[test]
    fn check_navigator_linear_order_map() {
        // going to gaurantee that navigator.nodes() comes in depth-first order of nested structure
        let kserd = Kserd::with_id(
            "0",
            Value::new_map(vec![
                (
                    Kserd::with_id("1", Value::new_num(0)).unwrap(),
                    Kserd::with_id(
                        "2",
                        Value::new_map(vec![(
                            Kserd::with_id("3", Value::Unit).unwrap(),
                            Kserd::with_id("4", Value::Unit).unwrap(),
                        )]),
                    )
                    .unwrap(),
                ),
                (
                    Kserd::with_id("5", Value::new_num(1)).unwrap(),
                    Kserd::with_id("6", Value::Unit).unwrap(),
                ),
                (
                    Kserd::with_id(
                        "7",
                        Value::new_map(vec![(
                            Kserd::with_id("8", Value::Unit).unwrap(),
                            Kserd::with_id("9", Value::Unit).unwrap(),
                        )]),
                    )
                    .unwrap(),
                    Kserd::with_id(
                        "10",
                        Value::new_map(vec![(
                            Kserd::with_id("11", Value::Unit).unwrap(),
                            Kserd::with_id("12", Value::Unit).unwrap(),
                        )]),
                    )
                    .unwrap(),
                ),
            ]),
        )
        .unwrap();

        let nav = Navigator::new(&kserd);

        assert_eq!(
            nav.root()
                .key_children()
                .map(|x| x.kserd().id().unwrap().to_string())
                .collect::<Vec<_>>(),
            ["1", "5", "7"]
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        );

        assert_eq!(nav.len(), 13);

        assert_eq!(
            nav.inner
                .iter()
                .map(|x| x.kserd.id().unwrap())
                .collect::<Vec<_>>(),
            &["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]
        );
    }

    #[test]
    fn empty_test() {
        let kserd = Kserd::new_unit();
        let nav = Navigator::new(&kserd);
        assert_eq!(nav.is_empty(), false);
    }
}
