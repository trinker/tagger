#' Convert Tags to Basic Tags
#'
#' Convert the Penn Treebank tags to basic part of speech tags.
#'
#' @details Basic tags include: (a) nouns, (b) adjectives, (c) prepositions,
#' (d) articles, (e) verbs, (f) pronouns, (g) adverbs, (h) interjections, &
#' (i) conjunctions.  The \code{X} and \code{.} tags are retained for punctuation
#' and unclassified parts of speech.  This tagset can be useful for more coarse
#' purposes, including formality (Heylighen & Dewaele, 2002) scoring.
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param \ldots ignored.
#' @return Returns a combined character vector of words and universal tags.
#' @export
#' @references Heylighen, F., & Dewaele, J.M. (2002). Variation in the
#' contextuality of language: An empirical measure. Context in Context, Special
#' issue of Foundations of Science, 7 (3), 293-340.
#' @keywords formality, explicit, parts-of-speech, pos
#' @export
#' @examples
#' (x <- tag_pos("They refuse to permit us to obtain the refuse permit"))
#' as_basic(x)
#'
#' (out1 <- tag_pos(sam_i_am))
#' as_basic(out1)
#'
#' presidential_debates_2012_pos
#' as_basic(presidential_debates_2012_pos)
#'
#' library(dplyr)
#' presidential_debates_2012_pos %>%
#'     as_basic() %>%
#'     count_tags(grouping.var=presidential_debates_2012[["person"]]) %>%
#'     plot()
as_basic <- function(x, ...) {

    out <- lapply(x, function(y){

        if (length(y) == 1 && is.na(y)) return(NA)
        names(y)[tolower(y) %in% c("the", "an", "a")] <- "article"
        names(y) <- .basic[match(names(y), .basic[["tag"]]), "basic"]
        y
    })

    class(out) <- c("tag_pos", class(out))
    out
}


.basic <- structure(list(tag = c("!", "#", "$", "''", "(", ")", ",", "-LRB-",
    "-RRB-", ".", ":", "?", "CC", "CD", "CD|RB", "DT", "EX", "FW",
    "IN", "IN|RP", "JJ", "JJR", "JJRJR", "JJS", "JJ|RB", "JJ|VBG",
    "LS", "MD", "NN", "NNP", "NNPS", "NNS", "NN|NNS", "NN|SYM", "NN|VBG",
    "NP", "PDT", "POS", "PRP", "PRP$", "PRP|VBP", "PRT", "RB", "RBR",
    "RBS", "RB|RP", "RB|VBG", "RN", "RP", "SYM", "TO", "UH", "VB",
    "VBD", "VBD|VBN", "VBG", "VBG|NN", "VBN", "VBP", "VBP|TO", "VBZ",
    "VP", "WDT", "WH", "WP", "WP$", "WRB", "``", "article"), basic = c(".",
    ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "conjunction",
    "adjective", "X", "adjective", "noun", "X", "preposition", "preposition",
    "adjective", "adjective", "adjective", "adjective", "adjective",
    "adjective", "X", "verb", "noun", "noun", "noun", "noun", "noun",
    "noun", "noun", "noun", "adjective", "X", "pronoun", "pronoun",
    "pronoun", "preposition", "adverb", "adverb", "adverb", "adverb",
    "adverb", "X", "preposition", "X", "preposition", "interjection",
    "verb", "verb", "verb", "verb", "verb", "verb", "verb", "verb",
    "verb", "verb", "pronoun", "X", "pronoun", "pronoun", "adverb",
    ".", "article")), .Names = c("tag", "basic"), row.names = c(NA, -69L), class = "data.frame")


