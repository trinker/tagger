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
#' count_tags(presidential_debates_2012_pos)
#' count_tags(presidential_debates_2012_pos, grouping.var = presidential_debates_2012$person)
#' with(presidential_debates_2012,
#'     count_tags(presidential_debates_2012_pos, grouping.var = list(person, time))
#' )
#'
#' library(dplyr)
#' presidential_debates_2012 %>%
#'     with(
#'         count_tags(
#'             presidential_debates_2012_pos,
#'             grouping.var = list(person, time)
#'         )
#'     ) %>%
#'     plot()
#'
#' presidential_debates_2012_pos %>%
#'     count_tags() %>%
#'     as_dtm()
#'
#' presidential_debates_2012_pos %>%
#'     as_basic() %>%
#'     count_tags() %>%
#'     as_dtm()
#'
#' presidential_debates_2012 %>%
#'     with(
#'         count_tags(
#'             presidential_debates_2012_pos,
#'             grouping.var = list(person, time)
#'         )
#'     ) %>%
#'     as_dtm()
count_tags <- function(x, grouping.var = NULL, group.names, pretty = TRUE, ...){

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
        out <- data.frame(group_dat, textshape::mtabulate(sapply(x, names)), check.names=FALSE)
        out <- data.table::setDT(out)[, lapply(.SD, sum, na.rm=TRUE), by=G]
        nms <- colnames(out)[!colnames(out) %in% G]
        out[, n.tokens := rowSums(out[, nms, with=FALSE], TRUE)]
        data.table::setcolorder(out, c(G, 'n.tokens', nms))

    } else {

        out <- textshape::mtabulate(sapply(x, names))
        out <- data.table::setDT(data.frame(out, check.names=FALSE))
        out[, n.tokens := rowSums(out, TRUE)]
        data.table::setcolorder(out, c('n.tokens', utils::head(colnames(out), -1)))
        G <- NULL
        nms <- colnames(out)[!colnames(out) %in% c(G, "n.tokens")]
    }

    class(out) <- c("count_tags", class(out))
    attributes(out)[["group.vars"]] <- G
    attributes(out)[["pos.vars"]] <- nms
    attributes(out)[["weight"]] <- "count"
    attributes(out)[["pretty"]] <- pretty
    attributes(out)[["term.vars"]] <- colnames(out)[!colnames(out) %in% c(G, "n.tokens")]
    return(out)
}


#' Prints a count_tags Object
#'
#' Prints a count_tags object.
#'
#' @param x The count_tags object.
#' @param digits The number of digits displayed.
#' @param weight The weight type.  Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.  See \code{\link[termco]{weight}} for
#' additional information.
#' @param zero.replace The value to replace zero count elements with; defaults
#' to \code{"0"}.
#' @param pretty logical.  If \code{TRUE} the counts print in a pretty fashion,
#' combining count and weighted information into a single display.
#' \code{pretty} printing can be permanently removed with
#' \code{\link[termco]{as_count}}.
#' @param \ldots ignored
#' @method print count_tags
#' @export
print.count_tags <- function (x, digits = 1, weight = "percent", zero.replace = "0",
    pretty = getOption("tagger_pretty"), ...) {

    n.tokens <- count <- NULL
    if (is.null(pretty)) pretty <- TRUE

    start <- Sys.time()

    termcols <- attributes(x)[["pos.vars"]]

    id <- FALSE
    if (is.null(attributes(x)[["group.vars"]])) {suppressWarnings(x[, id:=1:.N]); id <- TRUE}
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
    class(x) <- c(extra, class(x)[!class(x) %in% "count_tags"])

    print(x)
    ask <- getOption("tagger_pretty_ask")
    if (is.null(ask)) {
        ask <- TRUE
    }
    if (ask && ptime > 0.61 && interactive()) {
        message(paste0(paste(rep("=", 70), collapse = ""), "\n"),
            "\nYour `count_tags` object is larger and is taking a while to print.\n",
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



#' Plots a count_tags object
#'
#' Plots a count_tags object.
#'
#' @param x The count_tags object.
#' @param labels logical.  If \code{TRUE} the cell count values will be included
#' on the heatmap.
#' @param low The color to be used for lower values.
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NA} to remove the grid).
#' @param label.color The color to make labels if \code{labels = TRUE}.
#' @param label.size The size to make labels if \code{labels = TRUE}.
#' @param weight The weight to apply to the cell values for gradient fill.
#' Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.  See \code{\link[termco]{weight}} for
#' additional information.
#' @param \ldots ignored
#' @method plot count_tags
#' @export
plot.count_tags <- function(x, labels = FALSE, low ="white",
    high = "red", grid = NA, label.color = "grey70", label.size = 3,
    weight = "proportion", ...){

    group <- attributes(x)[["group.vars"]]
    y <- weight(x, weight = weight)

    y <- as.data.frame(rm_class(y, "data.table"), stringsAsFactors = FALSE)
    if (is.null(group)) {
        suppressWarnings(y[, 'group.vars' := rep("all", nrow(y))])
    } else {
        y[["group.vars"]] <- paste2(y[, group, drop=FALSE], sep = "_")
    }

    y <- y[!colnames(y) %in% group]
    vars <- colnames(y)[!colnames(y) %in% c("group.vars", "n.tokens")]
    dat <- tidyr::gather_(y, "terms", "values", vars)

    out <- ggplot2::ggplot(dat, ggplot2::aes_string(y = 'group.vars', x = "terms",
        fill = "values")) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(colour = "grey80"),
            legend.key.width = grid::unit(.25, 'cm'),
            legend.key.height = grid::unit(1, 'cm')
        ) +
        ggplot2::xlab("Part of Speech") +
        ggplot2::ylab("Groups") +
        ggplot2::geom_tile(color = grid) +
        ggplot2::scale_fill_gradient(high = high, low = low, name = "Percent",
            labels = scales::percent)

    if (isTRUE(labels)){
        values <- n.tokens <- NULL
        out <- out +
            ggplot2::geom_text(ggplot2::aes(label = round(n.tokens * values, 0)),
                color = label.color, size = label.size)
    }

    out
}



