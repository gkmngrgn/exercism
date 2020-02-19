import 'dart:math';

class Minesweeper {
  final String MINE = '*';
  final List<String> minefield;
  List<String> annoted = [];

  Minesweeper([this.minefield = const []]) {
    this.minefield.asMap().forEach((y, row) {
      List<String> annotatedRow = [];
      row.split('').asMap().forEach((x, c) {
        annotatedRow.add((c == MINE) ? MINE : this.count_mines(x, y));
      });
      this.annoted.add(annotatedRow.join());
    });
  }

  String count_mines(int x, int y) {
    var maxY = this.minefield.length - 1;
    var maxX = this.minefield[y].length - 1;
    var counter = 0;
    for (var y_ = max(y - 1, 0); y_ <= min(maxY, y + 1); y_++) {
      for (var x_ = max(x - 1, 0); x_ <= min(maxX, x + 1); x_++) {
        if (this.minefield[y_].split('')[x_] == MINE) {
          counter += 1;
        }
      }
    }
    return (counter == 0) ? ' ' : counter.toString();
  }
}
