final String scoreMap = '|AEIOULNRST|DG|BCMP|FHVWY|K|||JX||QZ';

int score(String word) => word.toUpperCase().split('').fold(
    0,
    (sum, letter) =>
        sum +
        scoreMap
            .split('')
            .takeWhile((l) => l != letter)
            .where((l) => l == '|')
            .length);
