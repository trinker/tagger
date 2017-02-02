#' Tidy Parts of Speech Object
#'
#' Pull out text element id, tokens, and parts of speech as separate columns in a
#' tidy data.frame.
#'
#' @param x A \code{tag_pos} object.
#' @param id.name The name for the column assigned to each element in the text vector.
#' @param token.name The name for the column of tokens.
#' @param pos.name The name for the column of parts of speech.
#' @param \ldots ignored.
#' @return Returns a \code{\link[data.table]{data.table}} with element, token,
#' and pos columns.
#' @export
#' @examples
#' (out1 <- tag_pos(sam_i_am))
#' tidy_pos(out1)
tidy_pos <- function (x, id.name = "id", token.name = "token", pos.name = "pos", ...) {

    stopifnot(methods::is(x, "tag_pos"))
    pos <- lapply(x, names)
    pos[sapply(pos, is.null)] <- NA

    dat <- data.frame(
        rep(seq_along(x), sapply(x, length)),
        unlist(x, use.names = FALSE),
        unlist(pos, use.names = FALSE),
        stringsAsFactors = FALSE,
        check.names = FALSE,
        row.names = NULL
    )
    colnames(dat) <- c(id.name, token.name, pos.name)
    data.table::data.table(dat)
}

