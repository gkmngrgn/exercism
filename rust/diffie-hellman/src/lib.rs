extern crate rand;

use rand::Rng;

fn modular_pow(mut base: u64, mut exponent: u64, modulus: u64) -> u64 {
    // rust pow accepts u32::max_value() as the maximum value. So we need
    // another optimized solution to calculate modular exponentiation. See
    // this link for more information:
    // https://en.wikipedia.org/wiki/Modular_exponentiation

    if modulus == 1 {
        return 0;
    }

    let mut result = 1;
    base = base % modulus;

    while exponent > 0 {
        if exponent % 2 == 1 {
            result = (result * base) % modulus
        }
        exponent >>= 1;
        base = base.pow(2) % modulus;
    }
    result
}

pub fn private_key(p: u64) -> u64 {
    rand::thread_rng().gen_range(1, p)
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    modular_pow(g, a, p)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    modular_pow(b_pub, a, p)
}
