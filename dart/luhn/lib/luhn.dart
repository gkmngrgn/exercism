class Luhn {
  bool valid(String cardNumber) {
    List<String> digits =
        cardNumber.replaceAll(" ", "").split("").reversed.toList();
    int digit;
    int total = 0;
    for (int i = 0; i < digits.length; i++) {
      digit = int.tryParse(digits[i]) ?? null;
      if (digit == null) return false;
      if (i % 2 != 0) digit *= 2;
      total += (digit > 9) ? digit - 9 : digit;
    }
    return digits.length > 1 && total % 10 == 0;
  }
}
