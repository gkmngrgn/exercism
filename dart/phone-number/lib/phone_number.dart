class PhoneNumber {
  String clean(String phoneNumber) {
    List<int> normalized = [];
    for (var letter in phoneNumber.split('')) {
      var number = int.tryParse(letter);
      if (number == null) {
        if (RegExp(r'[a-zA-Z]').hasMatch(letter)) {
          throw FormatException('letters not permitted');
        } else if (!['(', ')', '-', '+', ' ', '.'].contains(letter)) {
          throw FormatException('punctuations not permitted');
        }
      } else {
        normalized.add(number);
      }
    }

    if (normalized.length == 11) {
      if (normalized.first != 1)
        throw FormatException('11 digits must start with 1');
      normalized = normalized.skip(1).toList();
    } else if (normalized.length > 11)
      throw FormatException('more than 11 digits');
    else if (normalized.length < 10)
      throw FormatException('incorrect number of digits');

    if (normalized[0] == 0)
      throw FormatException('area code cannot start with zero');
    if (normalized[3] == 0)
      throw FormatException('exchange code cannot start with zero');
    if (normalized[0] == 1)
      throw FormatException('area code cannot start with one');
    if (normalized[3] == 1)
      throw FormatException('exchange code cannot start with one');
    return normalized.join('');
  }
}
