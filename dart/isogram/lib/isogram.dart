class Isogram {
  bool isIsogram(String word) {
    Iterable<String> letters =
        RegExp(r'[a-z]').allMatches(word.toLowerCase()).map((c) => c.group(0));
    return letters.length == letters.toSet().length;
  }
}
