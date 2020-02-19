import 'dart:math';

DateTime add(DateTime birthDate) =>
    birthDate.add(Duration(seconds: pow(10, 9).toInt()));
