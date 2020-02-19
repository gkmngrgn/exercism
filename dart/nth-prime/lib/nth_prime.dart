class NthPrime {
  int prime(int number) {
    var n = 1;
    if (number < n) throw ArgumentError('There is no zeroth prime');
    var currentNumber = 2;
    while (n < number) {
      currentNumber += 1;
      if (this.isPrime(currentNumber) == true) n += 1;
    }
    return currentNumber;
  }

  bool isPrime(int number) {
    if ([2, 3].contains(number)) return true;
    if (number % 2 == 0 || number % 3 == 0) return false;
    var divisor = 6;
    while (divisor * divisor - 2 * divisor + 1 <= number) {
      if (number % (divisor - 1) == 0 || number % (divisor + 1) == 0)
        return false;
      divisor += 6;
    }
    return true;
  }
}
