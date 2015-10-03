#' Penn Treebank Parts of Tags
#'
#' A list of Penn Treebank parts of tags and their meaning.
#'
#' @param print logical.  If \code{TRUE} a left justified pretty print version of the
#' data.frame is printed.
#' @return Invisibly returns a data frame of tags and meaning.
#' @export
#' @examples
#' penn_tags()
penn_tags <- function(print = TRUE){

     if (print) {
         print(left_just(penn_treebank_tags))
     }
     return(invisible(penn_treebank_tags))
}

penn_treebank_tags <- NLP::Penn_Treebank_POS_tags[c("entry", "description")]

