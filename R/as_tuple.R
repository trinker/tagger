#' Combine Words + Tags Tuple
#'
#' A traditional \pkg{Python} tuple style of displaying words and tags in the
#' same vector.
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param word.first logical.  If \code{TRUE} the word comes before the tag.
#' @param \ldots ignored.
#' @return Returns a lst of lists of tuples vectors..
#' @export
#' @note In order to generate a Python equivalent output use:\cr
#' \code{print(as_tuple(x), truncate=Inf, file="out.txt")}
#' @examples
#' (x <- tag_pos("I need $54 to go to the movies."))
#' c(x) ## The true structure of a `tag_pos` object
#' as_tuple(c(x))
#'
#' ## Don't truncate the printing
#' print(as_tuple(x), truncate=FALSE)
#'
#' ## Export tuple printing to file
#' \dontrun{
#' print(as_tuple(x), truncate=Inf, file="out.txt")
#' }
#' as_tuple(x, word.first=FALSE)
#'
#' (out1 <- tag_pos(sam_i_am))
#' as_tuple(c(out1))
as_tuple <- function(x, word.first = TRUE, ...){
    if (isTRUE(word.first)) {fun <- tuplefy1} else {fun <- tuplefy2}
    out <- lapply(x, fun)
    class(out) <- "as_tuple"
    out
}

#' Prints a as_tuple Object
#'
#' Prints a as_tuple object.
#'
#' @param x The as_tuple object.
#' @param truncate The number of characters to truncate the output.  Use \code{Inf}
#' to see all output.
#' @param \ldots Other arguments passed to \code{\link[base]{cat}}.  The \code{file}
#' argument is of particular use as the \code{as_tuple} object is a list of
#' vectors not as it prints.  Using \code{file} allows the user to export as it prints.
#' @method print as_tuple
#' @export
print.as_tuple <- function(x, truncate = 1500, ...){
    y <- lapply(x, function(y) paste0("[", paste(tupleprint(y), collapse=", "), "]"))
    y[sapply(y, function(x) x == "[NA]")] <- '[("", "")]'
    if (length(y) > 1) {
        y <- paste0("[", paste(unlist(y), collapse=", "), "]")
    }
    if(!is.infinite(truncate)) y <- paste0(substring(y, 1, truncate), "...")
    cat(y, ...)
}


tuplefy1 <- function(y) {
    if(length(y) == 1 && is.na(y)) return(NA)
    unname(Map(function(a, b) {c(a, b)}, y, names(y)))
}

tuplefy2 <- function(y) {
    if(length(y) == 1 && is.na(y)) return(NA)
    unname(Map(function(a, b) {c(b, a)}, y, names(y)))
}


tupleprint <- function(x){
    unlist(lapply(x, function(y) {
        if(length(y) == 1 && is.na(y)) return(NA)
        sprintf("(\"%s\", \"%s\")", y[1], y[2])
    }))
}
