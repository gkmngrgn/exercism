class Anagram {
  List<String> findAnagrams(String word, List<String> cluster) => cluster
      .where((c) => c.toLowerCase() != word.toLowerCase())
      .where((c) => this.sortLetters(c) == this.sortLetters(word))
      .toList();

  String sortLetters(String word) {
    var letters = word.toLowerCase().split('')..sort();
    return letters.join();
  }
}
