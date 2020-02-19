pub fn encode(n: u64) -> String {
    let mut encoded;
    if n < 20 {
        encoded = match n {
            0 => "zero",
            1 => "one",
            2 => "two",
            3 => "three",
            4 => "four",
            5 => "five",
            6 => "six",
            7 => "seven",
            8 => "eight",
            9 => "nine",
            10 => "ten",
            11 => "eleven",
            12 => "twelve",
            13 => "thirteen",
            14 => "fourteen",
            15 => "fifteen",
            16 => "sixteen",
            17 => "seventeen",
            18 => "eighteen",
            _ => "nineteen",
        }
        .to_string();
    } else if n < 100 {
        encoded = match n / 10 {
            2 => "twenty",
            3 => "thirty",
            4 => "forty",
            5 => "fifty",
            6 => "sixy",
            7 => "seventy",
            8 => "eighty",
            _ => "ninety",
        }
        .to_string();
        if n % 10 > 0 {
            encoded.push_str(&format!("-{}", &encode(n % 10)));
        }
    } else {
        let (digits, digits_str) = match n {
            n if n < 10_u64.pow(3) => (10_u64.pow(2), "hundred"),
            n if n < 10_u64.pow(6) => (10_u64.pow(3), "thousand"),
            n if n < 10_u64.pow(9) => (10_u64.pow(6), "million"),
            n if n < 10_u64.pow(12) => (10_u64.pow(9), "billion"),
            n if n < 10_u64.pow(15) => (10_u64.pow(12), "trillion"),
            n if n < 10_u64.pow(18) => (10_u64.pow(15), "quadrillion"),
            _ => (10_u64.pow(18), "quintillion"),
        };
        encoded = encode(n / digits);
        encoded.push_str(&format!(" {}", digits_str));
        if n % digits > 0 {
            encoded.push_str(&format!(" {}", &encode(n % digits)));
        }
    }
    encoded
}
