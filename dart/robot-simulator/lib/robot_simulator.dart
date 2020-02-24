import 'orientation.dart';
import 'position.dart';

class Robot {
  final int orientation_len = Orientation.values.length;
  Orientation orientation;
  Position position;

  Robot(this.position, this.orientation);

  void move(String instructions) => instructions.split('').forEach((i) {
        if (i == 'L') {
          this.turnLeft();
        } else if (i == 'R') {
          this.turnRight();
        } else if (i == 'A') {
          this.advance();
        }
      });

  void turnLeft() {
    this.orientation = Orientation.values[
        (orientation_len + this.orientation.index - 1) % orientation_len];
  }

  void turnRight() {
    this.orientation =
        Orientation.values[(this.orientation.index + 1) % orientation_len];
  }

  void advance() {
    if (this.orientation == Orientation.east) {
      this.position.x++;
    } else if (this.orientation == Orientation.west) {
      this.position.x--;
    } else if (this.orientation == Orientation.north) {
      this.position.y++;
    } else if (this.orientation == Orientation.south) {
      this.position.y--;
    }
  }
}
