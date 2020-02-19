use std::cmp::min;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    pub moves: u8,
    pub goal_bucket: Bucket,
    pub other_bucket: u8,
}

pub fn solve(
    capacity_1: u8,
    capacity_2: u8,
    goal: u8,
    start_bucket: &Bucket,
) -> Option<BucketStats> {
    let mut moves: Vec<Vec<u8>> = vec![];
    let (mut b_1, c_1, mut b_2, c_2) = match start_bucket {
        Bucket::One => (0, capacity_1, 0, capacity_2),
        Bucket::Two => (0, capacity_2, 0, capacity_1),
    };

    if goal == c_2 {
        // fill both buckets if goal is equal to other buckets capacity.
        b_1 = c_1;
        moves.push(vec![b_1, b_2]);
        b_2 = c_2;
        moves.push(vec![b_1, b_2]);
    }

    while b_1 != goal && b_2 != goal {
        if b_1 == 0 {
            b_1 = c_1; // fill
        } else if b_2 == c_2 {
            b_2 = 0; // empty
        } else {
            let poured_liter = min(c_2 - b_2, b_1);
            b_1 -= poured_liter; // pour
            b_2 += poured_liter;
        }

        if moves.contains(&vec![b_1, b_2]) {
            return None;
        }
        moves.push(vec![b_1, b_2]);
    }

    let (first_bucket, second_bucket) = match start_bucket {
        Bucket::One => (b_1, b_2),
        Bucket::Two => (b_2, b_1),
    };
    let (goal_bucket, other_bucket) = if goal == first_bucket {
        (Bucket::One, second_bucket)
    } else {
        (Bucket::Two, first_bucket)
    };

    return Some(BucketStats {
        moves: moves.len() as u8,
        goal_bucket: goal_bucket,
        other_bucket: other_bucket,
    });
}
