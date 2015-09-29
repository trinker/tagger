#' Title
#'
#' Description
#'
#' @param x
#' @param sep
#' @param \ldots
#' @return
#' @references
#' @keywords
#' @export
#' @seealso
#' @examples
as_tags <- function(x, sep = "/", ...){
    unlist(lapply(x, function(y) paste(paste(y, names(y), sep=sep), collapse = " ")))
}

