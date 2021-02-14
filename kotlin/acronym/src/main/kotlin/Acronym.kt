object Acronym {
    private val ignores: CharArray = charArrayOf(' ', '-', '_')

    fun generate(phrase: String): String =
        " $phrase".windowed(2)
            .filter { it[1].isLetter() && ignores.contains(it[0]) || it[1].isUpperCase() && it[0].isLowerCase() }
            .map { it[1] }.joinToString(separator = "").toUpperCase()
}
