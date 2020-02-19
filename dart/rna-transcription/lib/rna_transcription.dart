class RnaTranscription {
  final Map<String, String> rnaCodes = {'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'};

  String toRna(String strand) =>
      strand.split('').fold('', (x, y) => x + rnaCodes[y]);
}
