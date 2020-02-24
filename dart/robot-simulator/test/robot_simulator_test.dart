import 'package:robot_simulator/orientation.dart';
import 'package:robot_simulator/position.dart';
import 'package:robot_simulator/robot_simulator.dart';
import 'package:test/test.dart';

void main() {
  group('RobotSimulator', () {
    group('Create robot', () {
      test('at origin facing north', () {
        final Robot result = Robot(Position(0, 0), Orientation.north);
        expected(result, beAt: Position(0, 0), faceTo: Orientation.north);
      }, skip: false);

      test('at negative position facing south', () {
        final Robot result = Robot(Position(-1, -1), Orientation.south);
        expected(result, beAt: Position(-1, -1), faceTo: Orientation.south);
      }, skip: false);
    });

    group('Rotating clockwise', () {
      test('changes north to east', () {
        final Robot result = Robot(Position(0, 0), Orientation.north);
        result.move('R');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.east);
      }, skip: false);

      test('changes east to south', () {
        final Robot result = Robot(Position(0, 0), Orientation.east);
        result.move('R');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.south);
      }, skip: false);

      test('changes south to west', () {
        final Robot result = Robot(Position(0, 0), Orientation.south);
        result.move('R');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.west);
      }, skip: false);

      test('changes west to north', () {
        final Robot result = Robot(Position(0, 0), Orientation.west);
        result.move('R');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.north);
      }, skip: false);
    });

    group('Rotating counter-clockwise', () {
      test('changes north to west', () {
        final Robot result = Robot(Position(0, 0), Orientation.north);
        result.move('L');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.west);
      }, skip: false);

      test('changes west to south', () {
        final Robot result = Robot(Position(0, 0), Orientation.west);
        result.move('L');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.south);
      }, skip: false);

      test('changes south to east', () {
        final Robot result = Robot(Position(0, 0), Orientation.south);
        result.move('L');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.east);
      }, skip: false);

      test('changes east to north', () {
        final Robot result = Robot(Position(0, 0), Orientation.east);
        result.move('L');
        expected(result, beAt: Position(0, 0), faceTo: Orientation.north);
      }, skip: false);
    });

    group('Moving forward one', () {
      test('facing north increments Y', () {
        final Robot result = Robot(Position(0, 0), Orientation.north);
        result.move('A');
        expected(result, beAt: Position(0, 1), faceTo: Orientation.north);
      }, skip: false);

      test('facing south decrements Y', () {
        final Robot result = Robot(Position(0, 0), Orientation.south);
        result.move('A');
        expected(result, beAt: Position(0, -1), faceTo: Orientation.south);
      }, skip: false);

      test('facing east increments X', () {
        final Robot result = Robot(Position(0, 0), Orientation.east);
        result.move('A');
        expected(result, beAt: Position(1, 0), faceTo: Orientation.east);
      }, skip: false);

      test('facing west decrements X', () {
        final Robot result = Robot(Position(0, 0), Orientation.west);
        result.move('A');
        expected(result, beAt: Position(-1, 0), faceTo: Orientation.west);
      }, skip: false);
    });

    group('Follow series of instructions', () {
      test('moving east and north from README', () {
        final Robot result = Robot(Position(7, 3), Orientation.north);
        result.move('RAALAL');
        expected(result, beAt: Position(9, 4), faceTo: Orientation.west);
      }, skip: false);

      test('moving west and north', () {
        final Robot result = Robot(Position(0, 0), Orientation.north);
        result.move('LAAARALA');
        expected(result, beAt: Position(-4, 1), faceTo: Orientation.west);
      }, skip: false);

      test('moving west and south', () {
        final Robot result = Robot(Position(2, -7), Orientation.east);
        result.move('RRAAAAALA');
        expected(result, beAt: Position(-3, -8), faceTo: Orientation.south);
      }, skip: false);

      test('moving east and north', () {
        final Robot result = Robot(Position(8, 4), Orientation.south);
        result.move('LAAARRRALLLL');
        expected(result, beAt: Position(11, 5), faceTo: Orientation.north);
      }, skip: false);
    });
  });
}

void expected(Robot robot, {Position beAt, Orientation faceTo}) {
  expect(robot.position, equals(beAt));
  expect(robot.orientation, equals(faceTo));
}
