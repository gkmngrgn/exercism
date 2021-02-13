const val LETTER_COUNT: Int = 26

object Pangram {
    fun isPangram(input: String): Boolean = input.toLowerCase().toSet().filter { it.isLetter() }.count() == LETTER_COUNT
}
