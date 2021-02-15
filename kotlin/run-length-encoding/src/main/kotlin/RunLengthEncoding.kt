object RunLengthEncoding {

    fun encode(input: String): String {
        var output = ""
        var count = 1

        "$input ".windowed(2).forEach {
            val (curr, next) = it.toList()
            when (curr) {
                next -> count++
                else -> {
                    val code: String = when (count) {
                        1 -> curr.toString()
                        else -> "$count$curr"
                    }
                    output = "$output$code"
                    count = 1
                }
            }
        }
        if (count != 1) output = "$output${count - 1} "
        return output
    }

    fun decode(input: String): String {
        var output = ""
        var count = 0

        "$input ".windowed(2).forEach {
            val (curr, next) = it.toList()
            if (curr.isDigit()) {
                count = count * 10 + Integer.parseInt(curr.toString())
                if (!next.isDigit()) {
                    val text = next.toString().repeat(count - 1)
                    output = "$output$text"
                    count = 0
                }
            } else {
                output = "$output$curr"
            }
        }
        return output
    }

}
