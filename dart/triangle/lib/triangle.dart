class Triangle {
  int differentSides(num i, num j, num k) {
    var sum = i + j + k;
    var differentSides = [i, j, k].toSet();
    var sidesAreValid = [i, j, k].where((s) => s == 0 || sum - s < s).isEmpty;
    return (sidesAreValid) ? differentSides.length : 0;
  }

  bool equilateral(num i, num j, num k) => this.differentSides(i, j, k) == 1;

  bool isosceles(num i, num j, num k) =>
      [1, 2].contains(this.differentSides(i, j, k));

  bool scalene(num i, num j, num k) => this.differentSides(i, j, k) == 3;
}
