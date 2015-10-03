#' Combine Words + Tags
#'
#' A traditional style of displaying words and tags in the same vector.
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param sep A separator to put between the word and the tag.
#' @param word.first logical.  If \code{TRUE} the word comes before the tag.
#' @param \ldots ignored.
#' @return Returns a combined character vector of words and tags.
#' @export
#' @examples
#' (x <- tag_pos("I need $54 to go to the movies."))
#' c(x) ## The true structure of a `tag_pos` object
#' as_tags(c(x))
#' as_tags(c(x), word.first=FALSE)
#' as_tags(c(x), sep="|")
#' gsub("([^ ]+)", "(\\1)", as_tags(c(x), sep="="))
#'
#' (out1 <- tag_pos(sam_i_am))
#' c(out1)
#' as_tags(c(out1))
as_tags <- function(x, sep = "/", word.first = TRUE, ...){
    if (isTRUE(word.first)) {fun <- pastify1} else {fun <- pastify2}
    out <- unlist(lapply(x, function(y) paste(fun(y, sep=sep), collapse = " ")))
    out[out == "NA/"] <- NA
    out
}

pastify1 <- function(y, sep) paste(y, names(y), sep=sep)
pastify2 <- function(y, sep) paste(names(y), y, sep=sep)
