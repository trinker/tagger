#' Tag Text with Parts of Speech
#'
#' A wrapper for \pkg{NLP} and \pkg{openNLP} to easily tag text with parts of
#' speech.
#'
#' @param text.var The text string variable.
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default
#' is available cores minus one.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on
#' multiple cores.  Note that this may not mean a speed boost if you have one
#' core or if the data set is smaller as the cluster takes time to create.  The
#' function will not enable parallel unless the length of the text is sufficient
#' (calculated using \code{element chunks} and length of \code{text.var}).
#' @param element.chunks The number of elements to include in a chunk. Chunks are
#' passed through an \code{\link[base]{lapply}} and size is kept within a tolerance
#' because of memory allocation in the tagging process with \pkg{Java}.
#' @return Returns a list of part of speech tagged vectors.  The pretty printing
#' does not indicated this feature, but the words and parts of speech are easily
#' accessible through indexing.
#' @export
#' @examples
#' (x <- tag_pos("I need $54 to go to the movies."))
#' c(x) ## The true structure of a `tag_pos` object
#'
#' (out1 <- tag_pos(sam_i_am))
#' as_tags(out1)
#' count_pos(out1)
#' plot(out1)
#' \dontrun{
#' (out2 <- tag_pos(presidential_debates_2012$dialogue)) # ~40 sec run time
#' count_pos(out2)
#' count_pos(out2, by = presidential_debates_2012$person)
#' with(presidential_debates_2012, count_pos(out2, by = list(person, time)))
#' plot(out2)
#' }
tag_pos <- function(text.var, cores = parallel::detectCores() - 1,
    parallel = ifelse(cores > 2, TRUE, FALSE),
    element.chunks = floor(2000 * (23.5/mean(sapply(text.var, nchar), na.rm = TRUE)))){

    PTA <- openNLP::Maxent_POS_Tag_Annotator()
    ## Need pos and word token annotations.
    WTA <- openNLP::Maxent_Word_Token_Annotator()

    len <- length(text.var)

    ## locate empty or missing text elements
    nas <- sort(union(which(is.na(text.var)), grep("^\\s*$", text.var)))

    ## replace empty text with a period
    if(!identical(nas, integer(0))){
       text.var[nas] <- "."
    }

    ## Chunking the text into memory sized chunks:
    ## caluclate the start/end indexes of the chunks
    ends <- c(utils::tail(seq(0, by = element.chunks,
        length.out = ceiling(len/element.chunks)), -1), len)
    starts <- c(1, utils::head(ends + 1 , -1))

    ## chunk the text
    text_list <- Map(function(s, e) {text.var[s:e]}, starts, ends)

    ## Decide whether or not to use parallel procesing
    ## loop through the chunks and tag them
    if (parallel & length(text_list) > 8) {

        message("Using parallel process to tag.")
        cl <- parallel::makeCluster(mc <- getOption("cl.cores", cores))
        parallel::clusterExport(cl=cl, varlist=c("text_list", "tagPOS", "PTA",
            "WTA"), envir = environment())
        ## clusterEvalQ(cl, {require(NLP); require(openNLP)})
        out <- unlist(parallel::parLapply(cl, text_list, function(x) {
            x <- tagPOS(x, PTA, WTA)
            gc()
            return(x)
        }), recursive = FALSE)

        parallel::stopCluster(cl)

    } else {
        message("Not using parallel process to tag.")
        out <- unlist(lapply(text_list, function(x){
            x <- tagPOS(x, PTA, WTA)
            gc()
            x
        }), recursive = FALSE)
    }

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
#' @param n The number of rows to print (.5 * n goes to head and .5 * n goes to tail).
#' @param width The width of the tag rows to print.
#' @param \ldots ignored
#' @method print tag_pos
#' @export
print.tag_pos <- function(x, n = 10, width = .7 * getOption("width"), ...){
    n <- ceiling(10/2)
    y <- as_tags(x)
    if (length(y) <= 2*n) {
        print(y)
        #cat(sprintf("\n%s\n\nn = %s", paste(rep("*", 25), collapse="  "), length(y)))
    } else {
        top <- widther(utils::head(y, n), width=width)
        tails <- c(length(y) - (n-1)):length(y)
        bot <- widther(y[tails], width=width)

        numbs <- sprintf(paste0("%-", 1+nchar(length(y)), "s"), paste0(seq_along(y), "."))
        cat(paste(numbs[1:n], top, sep=" ", collapse="\n"))
        cat("\n.\n.\n.\n")

        cat(paste(numbs[tails], bot, sep=" ", collapse="\n"))
        #cat(sprintf("\n%s\nn = %s", paste(rep("-", 45), collapse=""), length(y)))
    }
}


#' Plots a tag_pos Object
#'
#' Plots a tag_pos object.
#'
#' @param x The tag_pos object
#' @param item.name The name of the variable that contains the groups (different
#' element in the vector/list).
#' @param \ldots Other arguments passed to \code{\link[termco]{plot_counts}}.
#' @method plot tag_pos
#' @export
plot.tag_pos <- function(x, item.name = "POS Tag", ...){
    y <- lapply(x, names)
    termco::plot_counts(y, item.name = item.name, ...)
}


widther <- function(x, width) {
    prime <- lapply(x, strwrap, width=width)
    y <- sapply(prime, "[[", 1)
    y[sapply(prime, length) > 1] <- paste(y[sapply(prime, length) > 1], "...")
    y
}


