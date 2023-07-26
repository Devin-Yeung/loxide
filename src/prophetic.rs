use std::collections::VecDeque;

pub trait Prophetic: Iterator {
    fn peek(&self) -> Option<Self::Item>;
    fn peek_next(&self) -> Option<Self::Item>;
}

pub struct Prophet<I: Iterator> {
    iter: I,
    buffer: VecDeque<Option<I::Item>>,
}

impl<I: Iterator> Prophet<I>
where
    I::Item: Copy,
{
    pub fn new(mut iter: I) -> Prophet<I> {
        let mut buffer = VecDeque::with_capacity(3);
        buffer.push_back(iter.next());
        buffer.push_back(iter.next());
        Prophet { iter, buffer }
    }

    pub fn peek(&self) -> Option<I::Item> {
        if self.buffer.len() < 1 {
            unreachable!()
        }
        self.buffer[0]
    }

    pub fn peek_next(&self) -> Option<I::Item> {
        if self.buffer.len() < 2 {
            unreachable!()
        }
        self.buffer[1]
    }

    fn fill(&mut self) {
        self.buffer.push_back(self.iter.next())
    }
}

impl<I: Iterator> Iterator for Prophet<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.buffer.push_back(self.iter.next());
        self.buffer.pop_front().flatten()
    }
}

impl<I: Iterator> Prophetic for Prophet<I>
where
    I::Item: Copy,
{
    fn peek(&self) -> Option<I::Item> {
        self.peek()
    }

    fn peek_next(&self) -> Option<I::Item> {
        self.peek_next()
    }
}

#[cfg(test)]
mod tests {
    use crate::prophetic::Prophet;

    #[test]
    fn peek() {
        let s = "Hello World!".to_string();
        let mut prophet = Prophet::new(s.chars());
        assert_eq!(prophet.peek(), Some('H'));
        assert_eq!(prophet.peek_next(), Some('e'));
        assert_eq!(prophet.next(), Some('H'));
        assert_eq!(prophet.peek(), Some('e'));
        assert_eq!(prophet.peek_next(), Some('l'));
        assert_eq!(prophet.next(), Some('e'));
        assert_eq!(prophet.next(), Some('l'));
        assert_eq!(prophet.peek(), Some('l'));
        assert_eq!(prophet.peek_next(), Some('o'));
    }
}
