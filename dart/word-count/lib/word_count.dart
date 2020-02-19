class WordCount {
  final RegExp re = RegExp(r"[a-z0-9]+(?:'[a-z]+)*");

  Map<String, int> countWords(String words) {
    Map<String, int> result = Map<String, int>();
    Iterable<Match> matches = re.allMatches(words.toLowerCase());
    matches.forEach((i) => result[i.group(0)] = (result[i.group(0)] ?? 0) + 1);
    return result;
  }
}
