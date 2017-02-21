#' Check the \pkg{Java}/Stanford \pkg{CoreNLP} Setup
#'
#' Check to make sure that \pkg{Java} is installed and of correct version and that
#' Stanford's \pkg{CoreNLP} is installed and in root.
#'
#' @param verbose If \code{TRUE} messages are printed even when everything is
#' installed.
#' @param \ldots Other arguments passed to \code{check_stanford_installed}.
#' @keywords setup
#' @export
#' @examples
#' \dontrun{
#' check_setup()
#' check_setup(FALSE)
#' }
check_setup <- function(verbose = TRUE, ...){
    check_java(verbose = verbose)
    check_stanford_installed(verbose = verbose, ...)
}

