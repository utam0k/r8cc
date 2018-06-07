use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stream {
    data: Vec<char>,
    pos: usize,
}

impl FromIterator<char> for Stream {
    fn from_iter<T: IntoIterator<Item = char>>(t: T) -> Self {
        Self {
            data: t.into_iter().collect(),
            pos: 0,
        }
    }
}

impl From<String> for Stream {
    fn from(s: String) -> Self {
        Self::from_iter(s.chars())
    }
}

impl Iterator for Stream {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.pos >= self.data.len() {
            return None;
        }
        self.pos += 1;
        Some(self.data[self.pos - 1])
    }
}

impl Stream {
    pub fn prev(&self) -> Self {
        self.jump(self.pos - 1)
    }

    pub fn jump(&self, pos: usize) -> Self {
        Self {
            data: self.data.clone(),
            pos: pos,
        }
    }

    pub fn skip_space(&mut self) {
        for c in self.data[self.pos..].iter() {
            self.pos += 1;
            if c.is_whitespace() {
                continue;
            }
            self.pos -= 1;
            return;
        }
    }
}
