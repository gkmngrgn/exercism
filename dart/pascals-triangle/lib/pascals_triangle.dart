class PascalsTriangle {
  List<List<int>> rows(int count) => Iterable<int>.generate(count).fold(
      [],
      (rows, i) => (rows.isEmpty)
          ? [
              [1]
            ]
          : rows + [this.getNextRow(rows.last, i + 1)]);

  List<int> getNextRow(List<int> row, int len) =>
      List<int>.generate(len, (x) => (row + [0])[x] + ([0] + row)[x]);
}
