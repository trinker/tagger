#' Title
#'
#' Description
#'
#' @param text.var
#' @param element.chunks
#' @return
#' @references
#' @keywords
#' @export
#' @seealso
#' @examples
tag_pos <- function(text.var,
    element.chunks = floor(2000 * (23.5/mean(sapply(text.var, nchar), rm.na = TRUE)))){

    len <- length(text.var)

    nas <- sort(union(which(is.na(text.var)), grep("^\\s*$", text.var)))


    if(!identical(nas, integer(0))){
       text.var[nas] <- "."
    }

    ends <- c(tail(seq(0, by = element.chunks, length.out = ceiling(len/element.chunks)), -1), len)
    starts <- c(1, head(ends + 1 , -1))

    text_list <- Map(function(s, e) {text.var[s:e]}, starts, ends)


    PTA <- openNLP::Maxent_POS_Tag_Annotator()

    out <- unlist(lapply(text_list, function(x){
        x <- tagPOS(x, PTA)
        gc()
        x
    }), recursive = FALSE)

    if(!identical(nas, integer(0))){
       out[nas] <- NA
    }

    class(out) <- c("tag_pos", class(out))
    out
}

#' Prints a tag_pos Object
#'
#' Prints a tag_pos object.
#'
#' @param x The tag_pos object.
#' @param n
#' @param \ldots ignored
#' @method print tag_pos
#' @export
print.tag_pos <- function(x, n = 4, ...){
    y <- as_tag(x)
    if (length(y) < 2*n) {
        print(y)
    } else {
        print(head(y, n))
        cat(".\n.\n.\n")
        print(y[c(length(y) - n):length(y)])
    }
}
