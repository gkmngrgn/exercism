object Hamming {
    fun compute(leftStrand: String, rightStrand: String): Int {
        if (leftStrand.length != rightStrand.length) {
            throw IllegalArgumentException("left and right strands must be of equal length")
        }
        return leftStrand.zip(rightStrand) { l, r ->
            when (l) {
                r -> 0
                else -> 1
            }
        }.sum()
    }
}
