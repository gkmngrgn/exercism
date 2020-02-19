import 'dart:math';

class DifferenceOfSquares {
  int squareOfSum(int number) =>
      pow(Iterable<int>.generate(number + 1).fold(0, (sum, n) => sum + n), 2)
          .toInt();

  int sumOfSquares(int number) => Iterable<int>.generate(number + 1)
      .fold(0, (sum, n) => sum + pow(n, 2).toInt());

  int differenceOfSquares(int number) =>
      squareOfSum(number) - sumOfSquares(number);
}
