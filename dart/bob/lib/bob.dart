class Bob {
  String response(String message) {
    String response;
    message = message.trim();
    if (message.length == 0) {
      response = 'Fine. Be that way!';
    } else if (message == message.toUpperCase() &&
        message.contains(RegExp(r'[a-zA-Z]'))) {
      if (message.endsWith('?')) {
        response = "Calm down, I know what I'm doing!";
      } else {
        response = 'Whoa, chill out!';
      }
    } else if (message.endsWith('?')) {
      response = 'Sure.';
    } else {
      response = 'Whatever.';
    }
    return response;
  }
}
