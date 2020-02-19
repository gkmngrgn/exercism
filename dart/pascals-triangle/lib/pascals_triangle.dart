class PascalsTriangle {
  List<List<int>> rows(int count) {
    List<List<int>> result = [];
    List<int> floor = [1];
    for (int i = 0; i < count; i++) {
      result.add(floor);
      floor = floor + [0];
      floor = List<int>.generate(
          floor.length, (x) => floor[x] + floor.reversed.toList()[x]);
    }
    return result;
  }
}
