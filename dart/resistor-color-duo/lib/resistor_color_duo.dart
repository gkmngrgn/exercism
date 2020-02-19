class ResistorColorDuo {
  List<String> colorCodes = [
    'black',
    'brown',
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'violet',
    'grey',
    'white'
  ];

  int value(List<String> colors) =>
      int.parse(colors.take(2).map((a) => colorCodes.indexOf(a)).join());
}
