use super::*;

impl<'a, 'k> NodeInner<'a, 'k> {
    pub fn value<'nav, 'node>(
        &'node self,
        navigator: &'nav Navigator<'a, 'k>,
    ) -> NodeValue<'a, 'k, 'nav, 'node> {
        match &self.kserd.val {
            Value::Tuple(_) => NodeValue::Tuple(SeqIter {
                seq: self.value_children.iter(),
                nav: navigator,
            }),
            Value::Cntr(v) => NodeValue::Cntr(NamedIter {
                names: v.keys(),
                values: self.value_children.iter(),
                nav: navigator,
            }),
            Value::Seq(_) => NodeValue::Seq(SeqIter {
                seq: self.value_children.iter(),
                nav: navigator,
            }),
            Value::Map(_) => NodeValue::Map(KeyedIter {
                keys: self.key_children.iter(),
                values: self.value_children.iter(),
                nav: navigator,
            }),
            _ => NodeValue::Primitive,
        }
    }
}

impl<'a, 'k, 'nav, 'node> SeqIter<'a, 'k, 'nav, 'node> {
    /// The number of items.
    pub fn len(&self) -> usize {
        self.seq.len()
    }

    /// The iterator has no elements.
    pub fn is_empty(&self) -> bool {
        self.seq.as_ref().is_empty()
    }
}

impl<'a: 'node, 'k: 'node, 'nav: 'node, 'node> Iterator for SeqIter<'a, 'k, 'nav, 'node> {
    type Item = Node<'a, 'k, 'nav, 'node>;

    fn next(&mut self) -> Option<Node<'a, 'k, 'nav, 'node>> {
        self.seq
            .next()
            .and_then(|&idx| self.nav.inner.get(idx))
            .map(|node| Node {
                node,
                nav: self.nav,
            })
    }
}

impl<'a, 'k, 'nav, 'node> NamedIter<'a, 'k, 'nav, 'node> {
    /// The number of items.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// The iterator has no elements.
    pub fn is_empty(&self) -> bool {
        self.values.as_ref().is_empty()
    }
}

impl<'a: 'node, 'k: 'node, 'nav: 'node, 'node> Iterator for NamedIter<'a, 'k, 'nav, 'node> {
    type Item = (&'node Kstr<'k>, Node<'a, 'k, 'nav, 'node>);

    fn next(&mut self) -> Option<(&'node Kstr<'k>, Node<'a, 'k, 'nav, 'node>)> {
        if let Some(name) = self.names.next() {
            self.values
                .next()
                .and_then(|&idx| self.nav.inner.get(idx))
                .map(|node| Node {
                    node,
                    nav: self.nav,
                })
                .map(|node| (name, node))
        } else {
            None
        }
    }
}

impl<'a, 'k, 'nav, 'node> KeyedIter<'a, 'k, 'nav, 'node> {
    /// The number of items.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// The iterator has no elements.
    pub fn is_empty(&self) -> bool {
        self.values.as_ref().is_empty()
    }
}

impl<'a: 'node, 'k: 'node, 'nav: 'node, 'node> Iterator for KeyedIter<'a, 'k, 'nav, 'node> {
    type Item = (Node<'a, 'k, 'nav, 'node>, Node<'a, 'k, 'nav, 'node>);

    fn next(&mut self) -> Option<(Node<'a, 'k, 'nav, 'node>, Node<'a, 'k, 'nav, 'node>)> {
        let key = self
            .keys
            .next()
            .and_then(|&idx| self.nav.inner.get(idx))
            .map(|node| Node {
                node,
                nav: self.nav,
            });
        if let Some(key) = key {
            self.values
                .next()
                .and_then(|&idx| self.nav.inner.get(idx))
                .map(|node| Node {
                    node,
                    nav: self.nav,
                })
                .map(|value| (key, value))
        } else {
            None
        }
    }
}
