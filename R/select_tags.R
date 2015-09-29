#' Title
#'
#' Description
#'
#' @param x
#' @param tags
#' @param negate
#' @param \ldots
#' @return
#' @references
#' @keywords
#' @export
#' @seealso
#' @examples
select_tags <- function(x, tags, negate = FALSE, ...){

    if (!isTRUE(negate)) {
        return(lapply(x, function(x){
            out <- x[names(x) %in% tags]
            if (identical(character(0), out)) return(NA)
            out
        }))
    }

    lapply(x, function(x){
        out <- x[!names(x) %in% tags]
        if (identical(character(0), out)) return(NA)
        out
    })
}

