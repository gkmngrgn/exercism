import 'dart:math';

class Darts {
  int score(num x, num y) {
    num distance = sqrt(pow(x, 2) + pow(y, 2));
    int score;
    if (distance <= 1)
      score = 10;
    else if (distance <= 5)
      score = 5;
    else if (distance <= 10)
      score = 1;
    else
      score = 0;
    return score;
  }
}
