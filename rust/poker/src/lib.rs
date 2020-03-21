use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
struct Hand<'a> {
    data: &'a str,
    counts: Vec<usize>,
    cards: Vec<usize>,
}

impl<'a> Hand<'a> {
    fn new(data: &'a str) -> Self {
        let mut status: HashMap<usize, usize> = HashMap::new();
        let mut suits = HashSet::new();
        data.split_whitespace().for_each(|card| {
            let (number_str, suit) = card.split_at(card.len() - 1);
            let number: usize = match number_str {
                "A" => 14,
                "K" => 13,
                "Q" => 12,
                "J" => 11,
                _ => number_str.parse::<usize>().unwrap(),
            };
            suits.insert(suit);
            *status.entry(number - 1).or_insert(0) += 1;
        });
        let mut status_vec: Vec<(usize, usize)> = status
            .into_iter()
            .map(|(card, count)| (count, card))
            .collect();
        status_vec.sort_by(|a, b| b.cmp(a));
        let (mut counts, mut cards) = status_vec.into_iter().unzip();
        if cards == vec![13, 4, 3, 2, 1] {
            cards = vec![4, 3, 2, 1, 0];
        }

        let is_flush = suits.len() == 1;
        let is_straight = cards.len() == 5
            && (1..5)
                .filter(|&i| cards.get(i).unwrap() + 1 == *cards.get(i - 1).unwrap())
                .count()
                == 4;

        if is_straight && is_flush {
            counts = vec![4, 1, 1];
        }
        if counts < vec![3, 2] {
            if is_flush {
                counts = vec![3, 1, 1, 2];
            }
            if is_straight {
                counts = vec![3, 1, 1, 1];
            }
        }
        Self {
            data,
            counts,
            cards,
        }
    }
}

impl<'a> PartialEq for Hand<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.counts == other.counts && self.cards == other.cards
    }
}

impl<'a> PartialOrd for Hand<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.counts.cmp(&other.counts) {
            Ordering::Equal => Some(self.cards.cmp(&other.cards)),
            ordering => Some(ordering),
        }
    }
}

pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut hands: Vec<Hand> = hands.iter().map(|h| Hand::new(h)).collect();
    hands.sort_by(|a, b| b.partial_cmp(a).unwrap());
    match &hands.clone().first() {
        Some(best_hand) => {
            let winning_hands: Vec<&'a str> = hands
                .into_iter()
                .filter(|hand| &hand == best_hand)
                .map(|hand| hand.data)
                .collect();
            Some(winning_hands)
        }
        _ => None,
    }
}
