tagPOS <-  function(text.var, PTA, WTA, ...) {

    #text.var <- gsub("-+", " ", text.var)
    text.var <- gsub("^\\s+|\\s+$", "", text.var)
    s <- NLP::as.String(paste(text.var, collapse=""))

    ## Manually calculate the starts and ends via nchar
    lens <- sapply(text.var, nchar)
    ends <- cumsum(lens)
    starts <- c(1, utils::head(ends + 1, -1))

    a2 <- NLP::Annotation(seq_along(starts), rep("sentence", length(starts)), starts, ends)
    a2 <- NLP::annotate(s, WTA, a2)
    a3 <- NLP::annotate(s, PTA, a2)

    ## Determine the distribution of POS tags for word tokens.
    a3w <- a3[a3$type == "word"]

    grab_starts <- which(a3w$start %in% starts)
    grab_ends <- which(a3w$end %in% ends)

    out <- stats::setNames(s[a3w], unlist(lapply(a3w$features, "[[", "POS")))
    Map(function(s, e){out[s:e]}, grab_starts, grab_ends)

}

weight <- function(x, weight = "percent", ...){

    switch(weight,
        percent = propify(x, perc),
        proportion = propify(x, prop),
        stop("Select an appropriate weighting method")
    )
}



rm_class <- function(x, remove = "count_tags", ...){
    class(x) <- class(x)[!class(x) %in% remove]
    x
}


propify <- function(x, fun, ...){

    validate_term_count(x)
    termcols <- attributes(x)[["term.vars"]]

    if (attributes(x)[["weight"]] != "count") {
        x <- attributes(x)[["counts"]]
    } else {
        counts <- new.env(FALSE)
        counts[["term_counts"]] <- x
        attributes(x)[["counts"]] <- counts
    }

    fun2 <- function(y) fun(y, x[["n.tokens"]])
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    dat <-x[, termcols]
    x[termcols] <- apply(dat, 2, fun2)
    class(x) <- c("tag_count_weighted", "data.table", "data.frame")
    attributes(x)[["weight"]] <- "proportion"
    x
}

prop <- function(a, b) a/b
perc <- function(a, b) 100*(a/b)


countfun <- function(x, y, ignore.case = TRUE){
    stringi::stri_count_regex(y, x,
        opts_regex=stringi::stri_opts_regex(case_insensitive = ignore.case))
}


comb <- function(a, b, digits, zero.replace = "0", weight = "percent") {
    const <- ifelse(weight == "percent", 100, 1)
    x <- sprintf("%s(%s%%)", a, digit_format(const * (a/b), digits))
    x[a == 0] <- zero.replace
    x
}

digit_format <- function (x, digits = getOption("digit_digits")) {
    if (is.null(digits)) digits <- 3
    if (length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }
    x <- round(as.numeric(x), digits)
    if (digits > 0)
        x <- sprintf(paste0("%.", digits, "f"), x)
    out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl = TRUE)
    out[out == "NA"] <- NA
    out
}


is.count <- function(x, ...){
    validate_term_count(x)
    attributes(x)[["weight"]] == "count"
}



paste2 <- function (multi.columns, sep = ".", handle.na = TRUE, trim = TRUE) {
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns, stringsAsFactors = FALSE)
    }
    if (trim)
        multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        })
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call("cbind", multi.columns)
    }
    if (handle.na) {
        m <- apply(multi.columns, 1, function(x) {
            if (any(is.na(x))) {
                NA
            } else {
                paste(x, collapse = sep)
            }
        })
    } else {
        m <- apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}

validate_term_count <- function(x, warn = FALSE){

    nms2 <- unlist(list(attributes(x)[["term.vars"]], "n.words"))
    nms <- unlist(list(attributes(x)[["group.vars"]], nms2))
    check <- all(nms %in% colnames(x)) && all(sapply(x[, nms2], is.numeric))
    check2 <- all(sapply(c("group.vars", "term.vars", "weight", "pretty"), function(y){
        !is.null(attributes(x)[[y]])
    }))
    check3 <- !any(colnames(x) %in% c(nms2, nms, "n.words"))
    if (!check | !check2 | check3) {
        if (isTRUE(warn)){
            warning("Does not appear to be a `term_count` object.\n",
                "  Has the object or column names been altered/added?",
                immediate. = TRUE
            )
        }
        return(FALSE)
    }
    TRUE
}

left_just <-
function(dataframe, column = NULL, keep.class = FALSE) {
    df.class <- function(dataframe) {
        sapply(1:ncol(dataframe), function(i) {
            x <- class(dataframe[, i])
            x[length(x)]
        })
    }
    CLASS <- df.class(dataframe)
    left.j <- function(x) {
        n <- max(nchar(x))
        return(sprintf(paste("%-", n, "s", sep = ""), x))
    }
    if (is.null(column)) column <- colnames(dataframe)
    lj <- function(DF2, column) {
        if (is.null(column)) column <- colnames(DF2)
        Q <- max(nchar(c(as.character(DF2[, column]), column)))
        DF2 <- data.frame(rbind(colnames(DF2), do.call(cbind,
            lapply(DF2, as.character))), check.names = FALSE)
        DF2[, column] <- left.j(as.character(DF2[, column]))
        if (is.character(column)) {
            col <- names(DF2)[which(names(DF2) == column)]
                names(DF2)[which(names(DF2) == column)] <- sprintf(paste("%-",
                Q, "s", sep = ""), col)
        } else {
            if (is.numeric(column)) {
                col <- names(DF2)[column]
                    names(DF2)[column] <- sprintf(paste("%-", Q, "s",
                    sep = ""), col)
            }
        }
        DF2 <- data.frame(DF2[-1, , drop = FALSE], check.names = FALSE)
        rownames(DF2) <- NULL
        return(DF2)
    }
    if (length(column) < 2) {
        if (!is.data.frame(dataframe)) {
            y <- as.character(substitute(dataframe))
            dataframe <- data.frame(dataframe, check.names = FALSE)
            y <- if (y[1]%in%c("[", "$")) y[2] else y[1]
            names(dataframe) <- y
        }
        DF3 <- lj(DF2=dataframe, column=column)
    } else {
        if (!is.numeric(column)) column <- match(column, names(dataframe))
        dat <- dataframe[, -c(column), drop=FALSE]
        ndf <- colnames(dataframe)
        LIST <- lapply(column, function(x) {
            lj(DF2=dataframe[, x, drop=FALSE], column = NULL)
        })
        dat2 <- data.frame(cbind(do.call('cbind', LIST), dat), checknames=FALSE)
        NAMES <- colnames(dat2)
        STrim <- function (x) gsub("^\\s+|\\s+$|\\.+$", "", x)
        newloc <- match(ndf, STrim(NAMES))
        DF3 <- dat2[, newloc]
    }
    if (keep.class) {
        colClasses <- function(d, colClasses) {
            colClasses <- rep(colClasses, len=length(d))
            d[] <- lapply(seq_along(d), function(i) switch(colClasses[i],
                numeric=as.numeric(d[[i]]),
                character=as.character(d[[i]]),
                Date=as.Date(d[[i]], origin='1970-01-01'),
                POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'),
                factor=as.factor(d[[i]]),
                methods::as(d[[i]], colClasses[i]) ))
            d
        }
        DF3 <- colClasses(DF3, CLASS)
    }
    colnames(DF3) <- gsub("\\.(?=\\.*$)", " ", colnames(DF3), perl=TRUE)
    return(DF3)
}
