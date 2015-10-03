#' Select Tags + Words From tag_pos Object
#'
#' Select part of speech tags and accompanying words.
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param tags The tags (or regex of \code{regex = TRUE} to select.
#' @param negate logical.  If \code{TRUE} and \code{regex = FALSE} the tags not
#' matching \code{tags} are returned.
#' @param regex logical.  If \code{TRUE} then \code{tag} will be grepped for as
#' a regex.
#' @param \ldots Other arguments passed to \code{\link[base]{grepl}}.
#' @return Returns a list of selected part of speech tagged vectors.
#' @export
#' @examples
#' data(presidential_debates_2012_pos)
#' library(dplyr)
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS"))
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
#'     count_tags(presidential_debates_2012[, c("person", "time")])
#'
#' presidential_debates_2012_pos %>%
#'     select_tags("NN", regex=TRUE) %>%
#'     count_tags(presidential_debates_2012[, c("person", "time")])
#'
#' presidential_debates_2012_pos %>%
#'     select_tags("NN(\\b|[^S])", regex=TRUE) %>%
#'     count_tags(presidential_debates_2012[, c("person", "time")])
#'
#' presidential_debates_2012_pos %>%
#'     select_tags(c("NN", "NNP", "NNPS", "NNS"), TRUE) %>%
#'     count_tags(presidential_debates_2012[, c("person", "time")])
select_tags <- function(x, tags, negate = FALSE, regex = FALSE, ...){

    if (isTRUE(regex)) {
        out <- lapply(x, function(x){
            out <- x[grepl(tags, names(x), perl = TRUE,  ...)]
            if (identical(character(0), out)) return(NA)
            out
        })
        class(out) <- c("tag_pos", class(out))
        return(out)
    }

    if (!isTRUE(negate)) {
        out <- lapply(x, function(x){
            out <- x[names(x) %in% tags]
            if (identical(character(0), out)) return(NA)
            out
        })
        class(out) <- c("tag_pos", class(out))
        return(out)
    }

    out <- lapply(x, function(x){
        out <- x[!names(x) %in% tags]
        if (identical(character(0), out)) return(NA)
        out
    })
    class(out) <- c("tag_pos", class(out))
    out
}

