use std::iter::Iterator;
use itertools::MultiPeek;

pub fn peek_n<A: Copy, B: Copy, I: Iterator<Item=(A, B, B)>>(it: &mut MultiPeek<I>, n: usize) -> Option<Vec<A>> {
    let mut collected = Vec::new();
    for _ in 0..n {
        match it.peek() {
            Some((chr, _, _)) => collected.push(*chr),
            None => return None,
        }
    }
    it.reset_peek();
    Some(collected)
}

pub fn take_n<A: Copy, B: Copy, I: Iterator<Item=(A, B, B)>>(it: &mut I, n: usize) -> Option<Vec<A>> {
    let mut collected = Vec::new();
    for _ in 0..n {
        match it.next() {
            Some((chr, _, _)) => collected.push(chr),
            None => return None,
        }
    }
    Some(collected)
}

pub fn advance_by<I: Iterator>(it: &mut I, n: usize) {
    for _ in 0..n {
        it.next();
    }
}

mod tests {
    use super::*;
    use itertools::multipeek;

    #[test]
    fn advance_by_n_test() {
        let mut it = (0..10).into_iter();
        assert_eq!(it.next().unwrap(), 0);
        advance_by(&mut it, 3);
        assert_eq!(it.next().unwrap(), 4);
    }

    #[test]
    fn peek_n_function() {
        let txt = "abcdefghijk".to_string();
        let mut it = multipeek(txt.chars().map(|c| (c, c, c)));

        assert_eq!(peek_n(&mut it, 4).unwrap(), vec!['a', 'b', 'c', 'd']);
        assert_eq!(it.next().unwrap().0, 'a');

        let second_it = multipeek(txt.chars().map(|c| (c, c, c)));
        assert!(peek_n(&mut it, txt.len() + 1).is_none());
    }

    #[test]
    fn take_n_function() {
        let txt = "abcdefghijk".to_string();
        let mut it = txt.chars().map(|c| (c, c, c));

        assert_eq!(take_n(&mut it, 4), Some(vec!['a', 'b', 'c', 'd']));
        assert_eq!(it.next().unwrap().0, 'e');
    }
}