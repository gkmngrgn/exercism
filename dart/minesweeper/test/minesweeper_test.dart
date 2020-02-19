import 'package:minesweeper/minesweeper.dart';
import 'package:test/test.dart';

final myMineSweeper = new Minesweeper();

void main() {
  group("MineSweeper: Simple cases - ", simpleTestCases);
  group("MineSweeper: Normal cases - ", normalTestCases);
  group("MineSweeper: Advanced cases - ", advancedTestCases);
}

void simpleTestCases() {
  test("no rows", () {
    List<String> result = new Minesweeper([]).annoted;
    expect(result, equals(<String>[]));
  });

  test("no columns", () {
    List<String> result = new Minesweeper([""]).annoted;
    expect(result, equals([""]));
  }, skip: false);

  test("no mines", () {
    List<String> result = new Minesweeper(["   ", "   ", "   "]).annoted;
    expect(result, equals(["   ", "   ", "   "]));
  }, skip: false);

  test("board with only mines", () {
    List<String> result = new Minesweeper(["***", "***", "***"]).annoted;
    expect(result, equals(["***", "***", "***"]));
  }, skip: false);
}

void normalTestCases() {
  test("mine surrounded by spaces", () {
    List<String> result = new Minesweeper(["   ", " * ", "   "]).annoted;
    expect(result, equals(["111", "1*1", "111"]));
  }, skip: false);

  test("space surrounded by mines", () {
    List<String> result = new Minesweeper(["***", "* *", "***"]).annoted;
    expect(result, equals(["***", "*8*", "***"]));
  }, skip: false);
}

void advancedTestCases() {
  test("horizontal line", () {
    List<String> result = new Minesweeper([" * * "]).annoted;
    expect(result, equals(["1*2*1"]));
  }, skip: false);

  test("horizontal line, mines at edges", () {
    List<String> result = new Minesweeper(["*   *"]).annoted;
    expect(result, equals(["*1 1*"]));
  }, skip: false);

  test("vertical line", () {
    List<String> result = new Minesweeper([" ", "*", " ", "*", " "]).annoted;
    expect(result, equals(["1", "*", "2", "*", "1"]));
  }, skip: false);

  test("vertical line, mines at edges", () {
    List<String> result = new Minesweeper(["*", " ", " ", " ", "*"]).annoted;
    expect(result, equals(["*", "1", " ", "1", "*"]));
  }, skip: false);

  test("cross", () {
    List<String> result =
        new Minesweeper(["  *  ", "  *  ", "*****", "  *  ", "  *  "]).annoted;
    expect(result, equals([" 2*2 ", "25*52", "*****", "25*52", " 2*2 "]));
  }, skip: false);

  test("large board", () {
    List<String> result = new Minesweeper(
        [" *  * ", "  *   ", "    * ", "   * *", " *  * ", "      "]).annoted;
    expect(result,
        equals(["1*22*1", "12*322", " 123*2", "112*4*", "1*22*2", "111111"]));
  }, skip: false);
}
