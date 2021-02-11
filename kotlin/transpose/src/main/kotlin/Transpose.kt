import kotlin.math.max

object Transpose {
    fun transpose(input: List<String>): List<String> {
        // FIXME: pseudocode
        val height = input.size
        var width = 0
        for (i in input) {
            width = max(width, i.length)
        }

        val result = mutableListOf<String>()
        for (x in 0 until width) {
            val chars = mutableListOf<Char?>()
            for (y in 0 until height) {
                val v = input[y].getOrNull(x)
                chars.add(v)
            }
            result.add(convertToLine(chars))
        }

        return result
    }

    private fun convertToLine(chars: List<Char?>): String {
        val len = chars.count() - chars.takeLastWhile { it == null }.count()
        return chars.take(len).map { it ?: ' ' }.joinToString(separator = "")
    }
}
