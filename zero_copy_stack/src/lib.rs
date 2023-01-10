//! A stack that can be manipulated without cloning while guaranteeing that the original stack is not modified.
//!
//! See [ZeroCopyStack] for more information.

/// A stack that can be manipulated without cloning while guaranteeing that the original stack is not modified.
///
/// To perform any operation on the stack you must acquire a [ZeroCopyStackHandle]. Handles can, in turn, have other
/// [ZeroCopyStackHandle]s created from them without ever cloning the underlying stack. Whenever a handle is dropped,
/// any element that was pushed to the stack using that handle is popped off the stack.
///
/// # Examples
///
/// ```
/// use zero_copy_stack::ZeroCopyStack;
///
/// let mut stack = ZeroCopyStack::new();
/// {
///     let mut handle = stack.handle();
///     handle.push(1);
///     handle.push(2);
///     handle.push(3);
///     assert_eq!(handle.len(), 3);
/// }
/// assert_eq!(stack.handle().len(), 0);
/// ```
///
/// Handles can also have other handles created from them:
///
/// ```
/// use zero_copy_stack::ZeroCopyStack;
///
/// let mut stack = ZeroCopyStack::new();
/// {
///     let mut handle = stack.handle();
///     handle.push(1);
///     handle.push(2);
///     handle.push(3);
///     {
///         let mut nested_handle = handle.handle();
///         nested_handle.push(4);
///         nested_handle.push(5);
///         nested_handle.push(6);
///         assert_eq!(nested_handle.len(), 6);
///     }
///     assert_eq!(handle.len(), 3);
/// }
/// assert_eq!(stack.handle().len(), 0);
/// ```
///
/// # Performance
///
/// A [ZeroCopyStack] is a simple wrapper around a [std::vec::Vec]. A [ZeroCopyStackHandle] is a simple wrapper
/// around a `&mut Vec` with a `Drop` implementation that truncates the vector to its original length. Hence, there is
/// almost no overhead to using a [ZeroCopyStack] and [ZeroCopyStackHandle]s over a [std::vec::Vec].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ZeroCopyStack<T> {
    stack: Vec<T>,
}

impl<T> ZeroCopyStack<T> {
    /// Creates a new, empty [ZeroCopyStack].
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Gets a new [ZeroCopyStackHandle] that can be used to manipulate the stack temporarily without cloning it.
    pub fn handle(&mut self) -> ZeroCopyStackHandle<T> {
        ZeroCopyStackHandle {
            starting_len: self.stack.len(),
            stack: &mut self.stack,
        }
    }

    /// Pushes a value onto the stack.
    pub fn push(&mut self, value: T) {
        self.stack.push(value)
    }

    /// Gets an element at a specific index in the stack starting from the bottom.
    pub fn get(&self, index: usize) -> Option<&T> {
        self.stack.get(index)
    }

    /// Returns the number of elements in the stack.
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    /// Returns `true` if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Returns an iterator over the elements in the stack starting from the bottom.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.stack.iter()
    }
}

impl<T> std::default::Default for ZeroCopyStack<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::iter::FromIterator<T> for ZeroCopyStack<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            stack: Vec::from_iter(iter),
        }
    }
}

impl<T> ZeroCopyStack<T>
where
    T: PartialEq,
{
    /// Searches for an element satisfying the predicate, starting from the top of the stack.
    pub fn find<P>(&self, predicate: P) -> Option<&T>
    where
        P: Fn(&T) -> bool,
    {
        self.stack.iter().rev().find(|x| predicate(x))
    }

    /// Returns `true` if the stack contains an element with the given value.
    pub fn contains(&self, x: &T) -> bool {
        self.stack.contains(x)
    }
}

/// A handle to a [ZeroCopyStack] that can be used to manipulate the stack temporarily without cloning it.
///
/// When the handle is dropped, the stack is reverted to its original state.
///
/// See [ZeroCopyStack] for more information.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ZeroCopyStackHandle<'input, T> {
    starting_len: usize,
    stack: &'input mut Vec<T>,
}

impl<'input, T> ZeroCopyStackHandle<'input, T> {
    /// Pushes a value onto the stack.
    pub fn push(&mut self, value: T) {
        self.stack.push(value)
    }

    /// Gets an element at a specific index in the stack starting from the bottom.
    pub fn get(&self, index: usize) -> Option<&T> {
        self.stack.get(index)
    }

    /// Returns the number of elements in the stack.
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    /// Returns `true` if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Returns the number of elements added to the stack using this handle.
    pub fn relative_len(&self) -> usize {
        self.stack.len() - self.starting_len
    }

    /// Returns an iterator over the elements in the stack starting from the bottom.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.stack.iter()
    }

    /// Gets a new [ZeroCopyStackHandle] from this handle.
    pub fn handle(&mut self) -> ZeroCopyStackHandle<T> {
        ZeroCopyStackHandle {
            starting_len: self.stack.len(),
            stack: self.stack,
        }
    }
}

impl<T> ZeroCopyStackHandle<'_, T>
where
    T: PartialEq,
{
    /// Searches for an element satisfying the predicate, starting from the top of the stack.
    pub fn find<P>(&self, predicate: P) -> Option<&T>
    where
        P: Fn(&T) -> bool,
    {
        self.stack.iter().rev().find(|x| predicate(x))
    }

    /// Returns `true` if the stack contains an element with the given value.
    pub fn contains(&self, x: &T) -> bool {
        self.stack.contains(x)
    }
}

impl<T> Drop for ZeroCopyStackHandle<'_, T> {
    fn drop(&mut self) {
        self.stack.truncate(self.starting_len);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_copy_stack_is_truncated_on_drop() {
        let mut stack = ZeroCopyStack::new();
        {
            let mut handle = stack.handle();
            handle.push(1);
            handle.push(2);
            handle.push(3);
            assert_eq!(handle.stack, &[1, 2, 3]);
        }
        assert_eq!(stack.stack, vec![]);
    }

    #[test]
    fn test_zero_copy_stack_can_have_nested_handles() {
        let mut stack = ZeroCopyStack::new();
        {
            let mut handle = stack.handle();
            handle.push(1);
            handle.push(2);
            handle.push(3);
            {
                let mut handle = handle.handle();
                handle.push(4);
                handle.push(5);
                handle.push(6);
                assert_eq!(handle.stack, &[1, 2, 3, 4, 5, 6]);
            }
            assert_eq!(handle.stack, &[1, 2, 3]);
        }
        assert_eq!(stack.stack, vec![]);
    }
}
