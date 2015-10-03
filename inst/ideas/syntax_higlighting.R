
x <- presidential_debates_2012_pos
grouping.var <- presidential_debates_2012$person
tags <- c(lightgreen="NN", pink="NNP", yellow="NNPS", orange="NNS")

hilight_pos(x, tags, grouping.var = NULL, regex = FALSE){

    if (is.null(names(tags))) stop("Must set names of `tags` as a color choice.  For example:\n\n   c(lightgreen=\"NN\", pink=\"NNP\")")

if (!isTRUE(regex)){
    tags <- setNames(paste0(tags, "\\b"), names(tags))
}

## What to search for
#regex <- c(statement = "\\.$", question = "\\?$", exclamation = "\\!$", trailing = "\\|$")

## What to call the searched for elements
marks_class <- names(tags)

## Map of color to named elements
    marks <- stats::setNames(paste0("x", sprintf(paste0("%0", nchar(length(tags)), "d"), seq_along(tags))), marks_class)


#marks <- c(statement = "lightgreen", question = "pink", exclamation = "orange",
#    trailing = "lightgray")

## Markup sentence by type
speech <- mark_regex(as_tags(x), tags, marks_class)

## Combine group variables and add <h> tags
person <- sapply(1:nrow(presidential_debates_2012), function(i) {
    x <- as.character(unlist(presidential_debates_2012[i, c("person", "time")]))
    h1(paste(x, collapse = ":"))
})

## Combine into the body
body <- p_(person, speech)

## Make a template with color mapping for mark markup
## Insert the body into the template
## Output the template to html
template2html(insert_body(highlight_template(marks), body))

## Open the html file
open_html()
