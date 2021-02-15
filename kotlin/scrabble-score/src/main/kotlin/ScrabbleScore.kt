object ScrabbleScore {
    private const val scoreMap: String = "|AEIOULNRST|DG|BCMP|FHVWY|K|||JX||QZ"

    private fun scoreLetter(c: Char): Int = scoreMap.takeWhile { it != c }.filter { it == '|' }.count()

    fun scoreWord(word: String): Int = word.toUpperCase().map { scoreLetter(it) }.sum()
}
