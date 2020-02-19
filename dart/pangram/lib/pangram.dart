class Pangram {
  bool isPangram(String text) {
    Set<String> letters = "abcdefghijklmnopqrstuvwxyz".split("").toSet();
    text.toLowerCase().split("").forEach((l) => letters.remove(l));
    return letters.length == 0;
  }
}
