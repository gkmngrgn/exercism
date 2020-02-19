import 'package:bob/bob.dart';
import 'package:test/test.dart';

final bob = Bob();

void main() {
  group('Bob', () {
    test('stating something', () {
      final String result = bob.response('Tom-ay-to, tom-aaaah-to.');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('shouting', () {
      final String result = bob.response('WATCH OUT!');
      expect(result, equals('Whoa, chill out!'));
    }, skip: false);

    test('shouting gibberish', () {
      final String result = bob.response('FCECDFCAAB');
      expect(result, equals('Whoa, chill out!'));
    }, skip: false);

    test('asking a question', () {
      final String result =
          bob.response('Does this cryogenic chamber make me look fat?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('asking a numeric question', () {
      final String result = bob.response('You are, what, like 15?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('asking gibberish', () {
      final String result = bob.response('fffbbcbeab?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('talking forcefully', () {
      final String result = bob.response('Hi there!');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('using acronyms in regular speech', () {
      final String result =
          bob.response('It\'s OK if you don\'t want to go work for NASA.');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('forceful question', () {
      final String result = bob.response('WHAT\'S GOING ON?');
      expect(result, equals('Calm down, I know what I\'m doing!'));
    }, skip: false);

    test('shouting numbers', () {
      final String result = bob.response('1, 2, 3 GO!');
      expect(result, equals('Whoa, chill out!'));
    }, skip: false);

    test('no letters', () {
      final String result = bob.response('1, 2, 3');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('question with no letters', () {
      final String result = bob.response('4?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('shouting with special characters', () {
      final String result =
          bob.response('ZOMG THE %^*@#\$(*^ ZOMBIES ARE COMING!!11!!1!');
      expect(result, equals('Whoa, chill out!'));
    }, skip: false);

    test('shouting with no exclamation mark', () {
      final String result = bob.response('I HATE THE DENTIST');
      expect(result, equals('Whoa, chill out!'));
    }, skip: false);

    test('statement containing question mark', () {
      final String result = bob.response('Ending with ? means a question.');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('non-letters with question', () {
      final String result = bob.response(':) ?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('prattling on', () {
      final String result =
          bob.response('Wait! Hang on. Are you going to be OK?');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('silence', () {
      final String result = bob.response('');
      expect(result, equals('Fine. Be that way!'));
    }, skip: false);

    test('prolonged silence', () {
      final String result = bob.response('          ');
      expect(result, equals('Fine. Be that way!'));
    }, skip: false);

    test('alternate silence', () {
      final String result = bob.response('\t\t\t\t\t\t\t\t\t\t');
      expect(result, equals('Fine. Be that way!'));
    }, skip: false);

    test('multiple line question', () {
      final String result =
          bob.response('\nDoes this cryogenic chamber make me look fat?\nNo.');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('starting with whitespace', () {
      final String result = bob.response('         hmmmmmmm...');
      expect(result, equals('Whatever.'));
    }, skip: false);

    test('ending with whitespace', () {
      final String result =
          bob.response('Okay if like my  spacebar  quite a bit?   ');
      expect(result, equals('Sure.'));
    }, skip: false);

    test('other whitespace', () {
      final String result = bob.response('\n\r \t');
      expect(result, equals('Fine. Be that way!'));
    }, skip: false);

    test('non-question ending with whitespace', () {
      final String result =
          bob.response('This is a statement ending with whitespace      ');
      expect(result, equals('Whatever.'));
    }, skip: false);
  });
}
