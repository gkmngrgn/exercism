import kotlin.math.roundToLong

class SpaceAge(private val seconds: Int) {
    private fun calculate(exp: Double): Double = (seconds / (31557600 * exp) * 100).roundToLong() / 100.0

    fun onEarth(): Double = calculate(1.0)
    fun onMercury(): Double = calculate(0.2408467)
    fun onVenus(): Double = calculate(0.61519726)
    fun onMars(): Double = calculate(1.8808158)
    fun onJupiter(): Double = calculate(11.862615)
    fun onSaturn(): Double = calculate(29.447498)
    fun onUranus(): Double = calculate(84.016846)
    fun onNeptune(): Double = calculate(164.79132)
}
