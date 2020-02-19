class PrimeFactors {
  List<int> factors(int number) {
    List<int> primeFactors = [];
    int divider = 2;
    while (number != 1) {
      if (number % divider != 0) {
        divider += 1;
        continue;
      }
      primeFactors.add(divider);
      number ~/= divider;
    }
    return primeFactors;
  }
}
