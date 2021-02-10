import kotlin.math.pow

object ResistorColorTrio {
    fun text(vararg input: Color): String {
        var value = ((getIndex(input[0]) * 10 + getIndex(input[1])) * 10.toDouble().pow(getIndex(input[2]))).toInt()
        var unitIndex = 0
        while (value % 1000 == 0) {
            value /= 1000
            unitIndex += 1
        }
        val unit = Unit.values()[unitIndex].toString().toLowerCase()
        return "$value $unit"
    }

    private fun getIndex(color: Color): Int = Color.values().indexOf(color)
}
