class MatchingBrackets {
  final Map<String, String> pairs = {'}': '{', ']': '[', ')': '('};

  Iterable<String> getBrackets(String text) =>
      RegExp(r'[{}\[\]()]').allMatches(text).map((m) => m.group(0));

  bool isPaired(String text) {
    List<String> unpaired = [];
    for (var bracket in this.getBrackets(text)) {
      if (!pairs.containsKey(bracket))
        unpaired.add(bracket);
      else if (unpaired.isEmpty || pairs[bracket] != unpaired.last)
        return false;
      else
        unpaired.removeLast();
    }
    return unpaired.isEmpty;
  }
}
