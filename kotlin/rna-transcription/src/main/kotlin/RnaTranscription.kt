const val DNA_CODES: String = "GCTA"
const val RNA_CODES: String = "CGAU"

fun transcribeToRna(dna: String): String = dna.map { RNA_CODES[DNA_CODES.indexOf(it)] }.joinToString(separator = "")
