#' Locate Part of Speech Tags
#'
#' Locate part of speech tags within a \code{tag_pos} object.
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param tags The tags (or regex of \code{regex = TRUE} to select.
#' @param negate logical.  If \code{TRUE} and \code{regex = FALSE} the tags not
#' matching \code{tags} are returned.
#' @param regex logical.  If \code{TRUE} then \code{tag} will be grepped for as
#' a regex.
#' @param logical logical.  If \code{TRUE} the out put while be logical \code{TRUE}/\code{FALSE}.
#' If \code{FALSE} output will be integer element indices.
#' @param \ldots Other arguments passed to \code{\link[base]{grepl}}.
#' @return Returns a list of locations (integer or logical) of part of speech
#' tags as vectors.
#' @export
#' @examples
#' data(presidential_debates_2012_pos)
#' library(dplyr)
#'
#' presidential_debates_2012_pos %>%
#'     locate_tags(c("NN", "NNP", "NNPS", "NNS"))
#'
#' presidential_debates_2012_pos %>%
#'     locate_tags(c("NN", "NNP", "NNPS", "NNS"), logical=FALSE)
#'
#' presidential_debates_2012_pos %>%
#'     locate_tags("NN", regex=TRUE)
#'
#' presidential_debates_2012_pos %>%
#'     locate_tags("NN(\\b|[^S])", regex=TRUE)
#'
#' presidential_debates_2012_pos %>%
#'     locate_tags(c("NN", "NNP", "NNPS", "NNS"), negate=TRUE)
locate_tags <- function(x, tags, negate = FALSE, regex = FALSE, logical = TRUE, ...){

    if (isTRUE(regex)) {
        out <- lapply(x, function(x){
            if (isTRUE(logical)) {fun <- grepl} else { fun <- grep}
            out <- fun(tags, names(x), perl = TRUE,  ...)
            if (identical(integer(0), out)) return(NA)
            out
        })
        return(out)
    }

    if (!isTRUE(negate)) {
        out <- lapply(x, function(x){
            out <- names(x) %in% tags
            if (!isTRUE(logical)) out <- which(out)
            if (identical(integer(0), out)) return(NA)
            out
        })
        return(out)
    }

    out <- lapply(x, function(x){
        out <- !names(x) %in% tags
        if (!isTRUE(logical)) out <- which(out)
        if (identical(integer(0), out)) return(NA)
        out
    })
    out
}

