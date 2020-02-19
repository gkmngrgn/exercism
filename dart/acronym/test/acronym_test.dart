import 'package:acronym/acronym.dart';
import 'package:test/test.dart';

final acronym = Acronym();

void main() {
  group('Acronym', () {
    group('Abbreviate a phrase', () {
      test('basic', () {
        final String result = acronym.abbreviate('Portable Network Graphics');
        expect(result, equals('PNG'));
      }, skip: false);

      test('lowercase words', () {
        final String result = acronym.abbreviate('Ruby on Rails');
        expect(result, equals('ROR'));
      }, skip: false);

      test('punctuation', () {
        final String result = acronym.abbreviate('First In, First Out');
        expect(result, equals('FIFO'));
      }, skip: false);

      test('all caps word', () {
        final String result =
            acronym.abbreviate('GNU Image Manipulation Program');
        expect(result, equals('GIMP'));
      }, skip: false);

      test('punctuation without whitespace', () {
        final String result =
            acronym.abbreviate('Complementary metal-oxide semiconductor');
        expect(result, equals('CMOS'));
      }, skip: false);

      test('very long abbreviation', () {
        final String result = acronym.abbreviate(
            'Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me');
        expect(result, equals('ROTFLSHTMDCOALM'));
      }, skip: false);

      test('consecutive delimiters', () {
        final String result =
            acronym.abbreviate('Something - I made up from thin air');
        expect(result, equals('SIMUFTA'));
      }, skip: false);

      test('apostrophes', () {
        final String result = acronym.abbreviate('Halley\'s Comet');
        expect(result, equals('HC'));
      }, skip: false);

      test('underscore emphasis', () {
        final String result = acronym.abbreviate('The Road _Not_ Taken');
        expect(result, equals('TRNT'));
      }, skip: false);
    });
  });
}
