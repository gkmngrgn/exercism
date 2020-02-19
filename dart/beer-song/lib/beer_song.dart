class BeerSong {
  List<String> recite(int start, int verseCount) =>
      Iterable<int>.generate(start + 1, (n) => start - n)
          .map((n) => this.verse(n, n == start - verseCount + 1))
          .take(verseCount)
          .expand((e) => e)
          .toList();

  List<String> verse(int n, bool isLast) {
    List<String> msg;
    if (n == 0) {
      msg = [
        'No more bottles of beer on the wall, no more bottles of beer.',
        'Go to the store and buy some more, 99 bottles of beer on the wall.'
      ];
    } else if (n == 1) {
      msg = [
        '1 bottle of beer on the wall, 1 bottle of beer.',
        'Take it down and pass it around, no more bottles of beer on the wall.'
      ];
    } else {
      msg = ['$n bottles of beer on the wall, $n bottles of beer.'];
      if (n == 2) {
        msg.add('Take one down and pass it around, '
            '1 bottle of beer on the wall.');
      } else {
        msg.add('Take one down and pass it around, '
            '${n - 1} bottles of beer on the wall.');
      }
    }
    if (!isLast) {
      msg.add('');
    }
    return msg;
  }
}
