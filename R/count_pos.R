#' Counts of Part of Speech Tags by Grouping Variable(s)
#'
#' Tabulate counts of part of speech tags by grouping variable(s).
#'
#' @param x A \code{tag_pos} object or a named list of vectors.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one row for each element in \code{x}.
#' @param group.names A vector of names that corresponds to group.var.  Generally
#' for internal use.
#' @param pretty logical.  If \code{TRUE} pretty printing is used.  Pretty
#' printing can be turned off globally by setting
#' \code{options(tagger_pretty = FALSE)}.
#' @param \ldots ignored
#' @return Returns a \code{\link[dplyr]{tbl_df}} of counts of parts of speech.
#' @keywords count proportion
#' @export
#' @importFrom data.table := .N .SD
#' @examples
#' data(presidential_debates_2012_pos) # pre tagged data
#' count_pos(presidential_debates_2012_pos)
#' count_pos(presidential_debates_2012_pos, by = presidential_debates_2012$person)
#' with(presidential_debates_2012,
#'     count_pos(presidential_debates_2012_pos, by = list(person, time))
#' )
count_pos <- function(x, grouping.var = NULL, group.names, pretty = TRUE, ...){

    n.tokens <- NULL

    if (!inherits(x, "tag_pos")) warning("Not a `tag_pos` object; results may be incorrect.")

    if (!is.null(grouping.var)){
        if (is.list(grouping.var) & length(grouping.var) > 1) {
            if (is.data.frame(grouping.var)) {
                grouping.var <- as.list(grouping.var)
                G <- names(grouping.var)
            } else {
                m <- unlist(as.character(substitute(grouping.var))[-1])
                G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                })
            }
            grouping <- grouping.var
        } else {
            #if (is.data.frame(grouping.var)) grouping.var <- as.list(grouping.var)
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
            grouping <- unlist(grouping.var)
        }

        if(!missing(group.names)) {
            G <- group.names
        }

        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)
        if(nrow(group_dat) != length(x)) stop("The `x` must be equal in length to `grouping.var`")
        out <- data.frame(group_dat, qdapTools::mtabulate(sapply(x, names)), check.names=FALSE)
        out <- data.table::setDT(out)[, lapply(.SD, sum, na.rm=TRUE), by=G]
        nms <- colnames(out)[!colnames(out) %in% G]
        out[, n.tokens := rowSums(out[, nms, with=FALSE], TRUE)]
        data.table::setcolorder(out, c(G, 'n.tokens', nms))

    } else {

        out <- qdapTools::mtabulate(sapply(x, names))
        out <- data.table::setDT(data.frame(out, check.names=FALSE))
        out[, n.tokens := rowSums(out, TRUE)]
        data.table::setcolorder(out, c('n.tokens', utils::head(colnames(out), -1)))
        G <- NULL
        nms <- colnames(out)[!colnames(out) %in% c(G, "n.tokens")]
    }

    class(out) <- c("count_pos", class(out))
    attributes(out)[["group.vars"]] <- G
    attributes(out)[["pos.vars"]] <- nms
    attributes(out)[["weight"]] <- "count"
    attributes(out)[["pretty"]] <- pretty
    return(out)
}


#' Prints a count_pos Object
#'
#' Prints a count_pos object.
#'
#' @param x The count_pos object.
#' @param digits The number of digits displayed.
#' @param weight The weight type.  Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.  See \code{\link[termco]{weight}} for
#' additional information.
#' @param zero.replace The value to replace zero count elements with; defaults
#' to \code{"0"}.
#' @param pretty logical.  If \code{TRUE} the counts print in a pretty fashion,
#' combining count and weighted information into a single display.
#' \code{pretty} printing can be permanantly removed with
#' \code{\link[termco]{as_count}}.
#' @param \ldots ignored
#' @method print count_pos
#' @export
print.count_pos <- function (x, digits = 1, weight = "percent", zero.replace = "0",
    pretty = getOption("tagger_pretty"), ...) {

    n.tokens <- count <- NULL
    if (is.null(pretty)) pretty <- TRUE

    start <- Sys.time()

    termcols <- attributes(x)[["pos.vars"]]

    id <- FALSE
    if (is.null(attributes(x)[["group.var"]])) {suppressWarnings(x[, id:=1:.N]); id <- TRUE}
    if (is.count(x) & pretty & attributes(x)[["pretty"]]) {
        tall <- tidyr::gather_(x, "term", "count", termcols)

        tall_weighted <- dplyr::mutate(tall, count = comb(count,
            n.tokens, digits = digits, zero.replace = zero.replace,
            weight = weight))
        x <- tidyr::spread_(tall_weighted, "term", "count")

        if (isTRUE(id)) {
            x <- dplyr::arrange_(x, "id")
            x[["id"]] <- NULL
        }
    }
    ptime <- difftime(Sys.time(), start)
    if(nrow(x) > 30){
        extra <- "tbl_df"
    } else {
        extra <- NULL
    }
    class(x) <- c(extra, class(x)[!class(x) %in% "count_pos"])

    print(x)
    ask <- getOption("tagger_pretty_ask")
    if (is.null(ask)) {
        ask <- TRUE
    }
    if (ask && ptime > 0.61 && interactive()) {
        message(paste0(paste(rep("=", 70), collapse = ""), "\n"),
            "\nYour `count_pos` object is larger and is taking a while to print.\n",
            "You can reduce this time grouping.var using `as_count` or setting:\n\n`options(tagger_pretty = FALSE)`\n\n",
            "Would you like to globally set `options(tagger_pretty = FALSE)` now?\n")
        ans <- utils::menu(c("Yes", "No", "Not Now"))
        switch(ans, `1` = {
            options(tagger_pretty = FALSE)
            options(tagger_pretty_ask = FALSE)
        }, `2` = {
            options(tagger_pretty_ask = FALSE)
        }, `3` = {
            options(tagger_pretty_ask = TRUE)
        })
    }
}

