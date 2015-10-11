#' Convert Tags to Universal Tags
#'
#' Convert the Penn Treebank tags to universal part of speech tags.
#'
#' @details Petrov, Das, & McDonald (2011) state that the universal tagset includes:
#'
#' \describe{
#'   \item{VERB}{verbs (all tenses and modes)}
#'   \item{NOUN}{nouns (common and proper)}
#'   \item{PRON}{pronouns }
#'   \item{ADJ}{adjectives}
#'   \item{ADV}{adverbs}
#'   \item{ADP}{adpositions (prepositions and postpositions)}
#'   \item{CONJ}{conjunctions}
#'   \item{DET}{determiners}
#'   \item{NUM}{cardinal numbers}
#'   \item{PRT}{particles or other function words}
#'   \item{X}{other: foreign words, typos, abbreviations}
#'   \item{.}{punctuation}
#' }
#'
#' For more see: \url{https://github.com/slavpetrov/universal-pos-tags}
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param tagset The name of a tagset dictionary to use as a key.  Use
#' \code{names(universal_pos_map)} to see possible choices.
#' @param dictionary A dataframe that maps the current tagset to a second tagset.
#' @param \ldots ignored.
#' @return Returns a combined character vector of words and universal tags.
#' @export
#' @references  Slav Petrov, Dipanjan Das and Ryan McDonald. (2011). A Universal Part-of-Speech Tagset. http://arxiv.org/abs/1104.2086
#' @examples
#' (x <- tag_pos("They refuse to permit us to obtain the refuse permit"))
#' as_universal(x)
#'
#' (out1 <- tag_pos(sam_i_am))
#' as_universal(out1)
#'
#' presidential_debates_2012_pos
#' as_universal(presidential_debates_2012_pos)
as_universal <- function(x, tagset = "en-ptb",
    dictionary = tagger::universal_pos_map, ...) {

    key <- dictionary[[tagset]]
    out <- lapply(x, function(y){
        names(y) <- key[match(names(y), key[["tag"]]), "universal"]
        y
    })
    class(out) <- c("tag_pos", class(out))
    out
}


