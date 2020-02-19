class SpaceAge {
  final Map<String, double> orbitalPeriods = {
    'Earth': 1,
    'Mercury': 0.2408467,
    'Venus': 0.61519726,
    'Mars': 1.8808158,
    'Jupiter': 11.862615,
    'Saturn': 29.447498,
    'Uranus': 84.016846,
    'Neptune': 164.79132
  };

  num age({String planet, int seconds}) => num.parse(
      ((seconds / 31557600) / orbitalPeriods[planet]).toStringAsFixed(2));
}
