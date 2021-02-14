class CustomSet(vararg initials: Int) {
    private var list: IntArray = initials

    fun isEmpty(): Boolean = list.isEmpty()

    fun isSubset(other: CustomSet): Boolean = isEmpty() || list.none { !other.contains(it) }

    fun isDisjoint(other: CustomSet): Boolean = isEmpty() || list.all { !other.contains(it) }

    fun contains(other: Int): Boolean = list.contains(other)

    fun intersection(other: CustomSet): CustomSet {
        val initials = list.intersect(other.list.asIterable()).toIntArray()
        return CustomSet(*initials)
    }

    fun add(other: Int) = list.plus(other).also { list = it }

    override fun equals(other: Any?): Boolean = when (other) {
        is CustomSet -> isSubset(other) && other.isSubset(this)
        else -> false
    }

    override fun hashCode(): Int = list.contentHashCode()

    operator fun plus(other: CustomSet): CustomSet = CustomSet(*list.plus(other.list.filter { !list.contains(it) }))

    operator fun minus(other: CustomSet): CustomSet = CustomSet(*list.filter { !other.contains(it) }.toIntArray())

}
