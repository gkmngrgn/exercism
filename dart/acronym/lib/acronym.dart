class Acronym {
  final RegExp re = RegExp(r"[a-zA-Z]+(?:'[a-z]+)*");

  String abbreviate(String value) =>
      re.allMatches(value).fold('', (x, y) => x + y.group(0)[0].toUpperCase());
}
