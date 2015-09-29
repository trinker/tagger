tagPOS <-  function(text.var, PTA, ...) {

    #text.var <- gsub("-+", " ", text.var)
    s <- NLP::as.String(paste(text.var, collapse=""))

    ## Set up the POS annotator if missing (for parallel)
    if (missing(PTA)) {
        PTA <- openNLP::Maxent_POS_Tag_Annotator()
    }

    ## Need sentence and word token annotations.
    word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()

    lens <- sapply(text.var, nchar)
    ends <- cumsum(lens)
    starts <- c(1, head(ends + 1, -1))

    a2 <- NLP::Annotation(seq_along(starts), rep("sentence", length(starts)), starts, ends)
    a2 <- NLP::annotate(s, word_token_annotator, a2)
    a3 <- NLP::annotate(s, PTA, a2)

    ## Determine the distribution of POS tags for word tokens.


    a3w <- a3[a3$type == "word"]

    grab_starts <- which(a3w$start %in% starts)
    grab_ends <- which(a3w$end %in% ends)

    out <- stats::setNames(s[a3w], unlist(lapply(a3w$features, "[[", "POS")))

    Map(function(s, e){out[s:e]}, grab_starts, grab_ends)

}


comb <- function (a, b, digits, zero.replace = "0", weight = "percent") {
    const <- ifelse(weight == "percent", 100, 1)
    x <- sprintf("%s(%s%%)", a, digit_format(const * (a/b), digits))
    x[a == 0] <- zero.replace
    x
}

digit_format <- function (x, digits = getOption("digit_digits")) {
    if (is.null(digits))
        digits <- 3
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
