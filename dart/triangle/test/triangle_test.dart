import 'package:test/test.dart';
import 'package:triangle/triangle.dart';

void main() {
  final triangle = new Triangle();

  group('Triangle', () {
    group('equilateral triangle', () {
      test('all sides are equal', () {
        final bool result = triangle.equilateral(2, 2, 2);
        expect(result, equals(true));
      }, skip: false);

      test('any side is unequal', () {
        final bool result = triangle.equilateral(2, 3, 2);
        expect(result, equals(false));
      }, skip: false);

      test('no sides are equal', () {
        final bool result = triangle.equilateral(5, 4, 6);
        expect(result, equals(false));
      }, skip: false);

      test('all zero sides is not a triangle', () {
        final bool result = triangle.equilateral(0, 0, 0);
        expect(result, equals(false));
      }, skip: false);

      test('sides may be floats', () {
        final bool result = triangle.equilateral(0.5, 0.5, 0.5);
        expect(result, equals(true));
      }, skip: false);
    });

    group('isosceles triangle', () {
      test('last two sides are equal', () {
        final bool result = triangle.isosceles(3, 4, 4);
        expect(result, equals(true));
      }, skip: false);

      test('first two sides are equal', () {
        final bool result = triangle.isosceles(4, 4, 3);
        expect(result, equals(true));
      }, skip: false);

      test('first and last sides are equal', () {
        final bool result = triangle.isosceles(4, 3, 4);
        expect(result, equals(true));
      }, skip: false);

      test('equilateral triangles are also isosceles', () {
        final bool result = triangle.isosceles(4, 4, 4);
        expect(result, equals(true));
      }, skip: false);

      test('no sides are equal', () {
        final bool result = triangle.isosceles(2, 3, 4);
        expect(result, equals(false));
      }, skip: false);

      test('first triangle inequality violation', () {
        final bool result = triangle.isosceles(1, 1, 3);
        expect(result, equals(false));
      }, skip: false);

      test('second triangle inequality violation', () {
        final bool result = triangle.isosceles(1, 3, 1);
        expect(result, equals(false));
      }, skip: false);

      test('third triangle inequality violation', () {
        final bool result = triangle.isosceles(3, 1, 1);
        expect(result, equals(false));
      }, skip: false);

      test('sides may be floats', () {
        final bool result = triangle.isosceles(0.5, 0.4, 0.5);
        expect(result, equals(true));
      }, skip: false);
    });

    group('scalene triangle', () {
      test('no sides are equal', () {
        final bool result = triangle.scalene(5, 4, 6);
        expect(result, equals(true));
      }, skip: false);

      test('all sides are equal', () {
        final bool result = triangle.scalene(4, 4, 4);
        expect(result, equals(false));
      }, skip: false);

      test('two sides are equal', () {
        final bool result = triangle.scalene(4, 4, 3);
        expect(result, equals(false));
      }, skip: false);

      test('may not violate triangle inequality', () {
        final bool result = triangle.scalene(7, 3, 2);
        expect(result, equals(false));
      }, skip: false);

      test('sides may be floats', () {
        final bool result = triangle.scalene(0.5, 0.4, 0.6);
        expect(result, equals(true));
      }, skip: false);
    });
  });
}
