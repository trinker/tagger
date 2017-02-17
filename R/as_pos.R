#' Extract Parts of Speech or Tokens from a \code{tag_pos} Object
#'
#' Extract parts of speech or corresponding tokens from a
#' \code{\link[tagger]{tag_pos}} object.  This is useful for extracting
#' part of speech based text features for modeling.
#'
#' @param x A \code{\link[tagger]{tag_pos}} object.
#' @param collapse logical.  If \code{TRUE} the pos/tokens will be collapsed
#' into a vector of paste collapsed strings.  If \code{FALSE} the pos/tokens
#' will be returned as a list of vectors.
#' @param \ldots ignored.
#' @return Returns a vector of strings or a list of pos/tokens.
#' @export
#' @rdname as_pos
#' @examples
#' library(dplyr)
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
#'     as_pos()
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
#'     as_pos(collapse = FALSE)
#'
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
#'     as_tokens()
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
#'     as_tokens(collapse = FALSE)
as_pos <- function(x, collapse = TRUE, ...) {
    stopifnot('tag_pos' %in% class(x))
    if (!isTRUE(collapse)) return(sapply(x, names))
    sapply(x, function(x) {
        if (length(x) == 0 | length(x) == 1 && is.na(x)) return(NA)
        paste(names(x), collapse = " ")
    })
}


#' @export
#' @rdname as_pos
as_tokens <- function(x, collapse = TRUE, ...) {
    stopifnot('tag_pos' %in% class(x))
    if (!isTRUE(collapse)) return(sapply(x, unname))
    out <- unlist(lapply(x, function(x) {
        if (length(x) == 0 | length(x) == 1 && is.na(x)) return(NA)
        paste(x, collapse = " ")
    }))
    gsub("(\\s+)([.!?,;:])", "\\2", out, perl = TRUE)
}

