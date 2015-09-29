#' Title
#'
#' Description
#'
#' @param print
#' @return
#' @references
#' @keywords
#' @export
#' @seealso
#' @examples
penn_tags <- function(print = TRUE){

     if (print) {
         print(left_just(penn_treebank_tags))
         message("\n\n*Taken from University of Leeds: http://www.comp.leeds.ac.uk/amalgam/tagsets/upenn.html")
     }
     return(invisible(penn_treebank_tags))
}


penn_treebank_tags <- structure(list(Tag = structure(c(3L, 9L, 1L, 4L, 5L, 6L, 2L,
    7L, 8L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L,
    21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L,
    34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L), .Label = c("''",
    "--", "$", "(", ")", ",", ".", ":", "``", "CC", "CD", "DT", "EX",
    "FW", "IN", "JJ", "JJR", "JJS", "LS", "MD", "NN", "NNP", "NNPS",
    "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP",
    "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT",
    "WP", "WP$", "WRB"), class = "factor"), Description = structure(c(15L,
    28L, 9L, 27L, 8L, 11L, 13L, 34L, 10L, 12L, 26L, 14L, 16L, 17L,
    31L, 2L, 3L, 4L, 20L, 21L, 23L, 25L, 24L, 22L, 30L, 18L, 32L,
    33L, 5L, 6L, 7L, 29L, 35L, 1L, 19L, 36L, 38L, 39L, 37L, 41L,
    40L, 43L, 44L, 45L, 42L), .Label = c("\"to\" as preposition or infinitive marker",
    "adjective or numeral, ordinal", "adjective, comparative", "adjective, superlative",
    "adverb", "adverb, comparative", "adverb, superlative", "closing parenthesis",
    "closing quotation mark", "colon or ellipsis", "comma", "conjunction, coordinating",
    "dash", "determiner", "dollar", "existential there", "foreign word",
    "genitive marker", "interjection", "list item marker", "modal auxiliary",
    "noun, common, plural", "noun, common, singular or mass", "noun, proper, plural",
    "noun, proper, singular", "numeral, cardinal", "opening parenthesis",
    "opening quotation mark", "particle", "pre-determiner", "preposition or conjunction, subordinating",
    "pronoun, personal", "pronoun, possessive", "sentence terminator",
    "symbol", "verb, base form", "verb, past participle", "verb, past tense",
    "verb, present participle or gerund", "verb, present tense, 3rd person singular",
    "verb, present tense, not 3rd person singular", "Wh-adverb",
    "WH-determiner", "WH-pronoun", "WH-pronoun, possessive"), class = "factor")), .Names = c("Tag",
    "Description"), class = "data.frame", row.names = c(NA, -45L))

