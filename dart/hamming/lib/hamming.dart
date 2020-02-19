class Hamming {
  int distance(String first, String second) {
    if (first.length != second.length) {
      if (first.length == 0 || second.length == 0)
        throw ArgumentError('no strand must be empty');
      else
        throw ArgumentError('left and right strands must be of equal length');
    }
    return Iterable<int>.generate(first.length)
        .where((i) => first[i] != second[i])
        .length;
  }
}
