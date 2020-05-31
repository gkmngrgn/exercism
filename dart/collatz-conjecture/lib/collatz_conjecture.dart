class CollatzConjecture {
  int steps(int n) {
    if (n <= 0) throw new ArgumentError("Only positive numbers are allowed");
    int steps = 0;
    while (n != 1) {
      n = n % 2 == 0 ? n ~/ 2 : 3 * n + 1;
      steps += 1;
    }
    return steps;
  }
}
