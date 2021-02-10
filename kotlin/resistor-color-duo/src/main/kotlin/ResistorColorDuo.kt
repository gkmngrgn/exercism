object ResistorColorDuo {
    fun value(vararg colors: Color): Int =
        Integer.parseInt(colors.take(2).map { Color.values().indexOf(it) }.joinToString(separator = ""))
}
