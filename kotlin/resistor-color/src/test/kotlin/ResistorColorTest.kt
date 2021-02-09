import kotlin.test.Test
import kotlin.test.assertEquals

class ResistorColorTest {

    @Test
    fun testBlackColorCode() = assertEquals(0, ResistorColor.colorCode("black"))

    @Test
    fun testWhiteColorCode() = assertEquals(9, ResistorColor.colorCode("white"))

    @Test
    fun testOrangeColorCode() = assertEquals(3, ResistorColor.colorCode("orange"))

    @Test
    fun testColors() {
        val expected = listOf("black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white")
        assertEquals(expected, ResistorColor.colors())
    }
}
