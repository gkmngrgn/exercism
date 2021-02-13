import kotlin.math.pow
import kotlin.math.sqrt

object Darts {
    fun <T> score(x: T, y: T): Int
        where T : Number {
        return when (sqrt(getValue(x).pow(2.0) + getValue(y).pow(2.0))) {
            in 0.0..1.0 -> 10
            in 1.0..5.0 -> 5
            in 5.0..10.0 -> 1
            else -> 0
        }
    }

    fun getValue(n: Number): Double {
        // FIXME
        return when (n is Int) {
            true -> n.toDouble()
            else -> n as Double
        }
    }
}
