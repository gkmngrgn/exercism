import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import kotlin.math.pow


class Gigasecond(startDate: LocalDateTime) {
    constructor(startDate: LocalDate) : this(startDate = LocalDateTime.of(startDate, LocalTime.MIN))

    val date: LocalDateTime = startDate.plusSeconds(10.0.pow(9).toLong())
}
