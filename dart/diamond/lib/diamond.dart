class Diamond {
  final int charCodeA = 65;

  List<String> rows(String letter) {
    var index = letter.codeUnitAt(0) - charCodeA;
    List<String> sequence = Iterable<int>.generate(index + 1).map((i) {
      String line =
          ' ' * (index - i) + String.fromCharCode(charCodeA + i) + ' ' * i;
      return line + line.split('').reversed.skip(1).join();
    }).toList();
    return [sequence, sequence.reversed.skip(1)].expand((p) => p).toList();
  }
}
