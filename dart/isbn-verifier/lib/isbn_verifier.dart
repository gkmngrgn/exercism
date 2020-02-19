bool isValid(String isbn) {
  if (RegExp(r"^[0-9-]{9,}?[0-9X]{1}$").stringMatch(isbn) != isbn) return false;
  List<String> digits = isbn.replaceAll("-", "").split("").reversed.toList();
  int total = 0;
  for (int i = 0; i < digits.length; i++) {
    total += (int.tryParse(digits[i]) ?? 10) * (i + 1);
  }
  return total % 11 == 0 && digits.length == 10;
}
