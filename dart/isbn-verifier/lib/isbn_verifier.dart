bool isValid(String isbn) {
  if (RegExp(r'^[0-9-]{9,}?[0-9X]{1}$').stringMatch(isbn) != isbn) return false;
  var digits = isbn.replaceAll('-', '').split('').reversed.toList();
  var total = digits
      .asMap()
      .map((i, d) => MapEntry(i, (int.tryParse(d) ?? 10) * (i + 1)))
      .values
      .reduce((a, b) => a + b);
  return total % 11 == 0 && digits.length == 10;
}
