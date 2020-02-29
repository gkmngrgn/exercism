class SecretHandshake {
  List<String> commands(int i) {
    List<String> result = [];
    if (i & (1 << 0) > 0) {
      result.add('wink');
    }
    if (i & (1 << 1) > 0) {
      result.add('double blink');
    }
    if (i & (1 << 2) > 0) {
      result.add('close your eyes');
    }
    if (i & (1 << 3) > 0) {
      result.add('jump');
    }
    if (i & (i << 4) > 0) {
      result = result.reversed.toList();
    }
    return result;
  }
}
