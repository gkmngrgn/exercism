const Map<String, int> Allergens = {
  'eggs': 1,
  'peanuts': 2,
  'shellfish': 4,
  'strawberries': 8,
  'tomatoes': 16,
  'chocolate': 32,
  'pollen': 64,
  'cats': 128,
};

class Allergies {
  bool allergicTo(String allergen, int score) =>
      Allergens[allergen] & score > 0;

  List<String> list(int score) =>
      Allergens.keys.where((a) => this.allergicTo(a, score)).toList();
}
