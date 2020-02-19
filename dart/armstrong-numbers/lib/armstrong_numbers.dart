import 'dart:math';

class ArmstrongNumbers {
  bool isArmstrongNumber(num number) =>
      number.toString().split('').fold(0,
          (num sum, i) => sum + pow(num.parse(i), number.toString().length)) ==
      number;
}
