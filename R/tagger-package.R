#' Part of Speech Tagging
#'
#' Wraps the \pkg{NLP} and \pkg{openNLP} packages for easier part of speech tagging.
#' @docType package
#' @name tagger
#' @aliases tagger package-tagger
NULL


#' 2012 U.S. Presidential Debates
#'
#' A dataset containing a cleaned version of all three presidential debates for
#' the 2012 election.
#'
#' @details
#' \itemize{
#'   \item person. The speaker
#'   \item tot. Turn of talk
#'   \item dialogue. The words spoken
#'   \item time. Variable indicating which of the three debates the dialogue is from
#' }
#'
#' @docType data
#' @keywords datasets
#' @name presidential_debates_2012
#' @usage data(presidential_debates_2012)
#' @format A data frame with 2912 rows and 4 variables
NULL



#' Sam I Am Text
#'
#' A dataset containing a character vector of the text from Seuss's 'Sam I Am'.
#'
#' @docType data
#' @keywords datasets
#' @name sam_i_am
#' @usage data(sam_i_am)
#' @format A character vector with 169 elements
#' @references Seuss, Dr. (1960). Green Eggs and Ham.
NULL


#' Parts of Speech for 2012 U.S. Presidential Debates Dialogue
#'
#' A dataset containing a list of named vectors of words.  The word names are the
#' part of speech tags.
#'
#' @docType data
#' @keywords datasets
#' @name presidential_debates_2012_pos
#' @usage data(presidential_debates_2012_pos)
#' @format A list with 2912 elements
NULL

#' Mapping to Universal Tags
#'
#' A dataset containing a list \code{\link[base]{data.frame}} maps to move from
#' Penn Treebank tagsets to a set of 12 universal part of speech tags.  This
#' mapping comes from Petrov, Das, & McDonald (2011).
#'
#' @details Each \code{\link[base]{data.frame}} contains a tag column,
#' corresponding to the Penn Treebank tagset, and a universal column,
#' corresponding to the 12 universal tags.
#'
#'
#' @docType data
#' @keywords datasets
#' @name universal_pos_map
#' @usage data(universal_pos_map)
#' @format A list with 33 elements
#' @references \url{https://github.com/slavpetrov/universal-pos-tags}
#'
#' Slav Petrov, Dipanjan Das and Ryan McDonald. (2011). A Universal Part-of-Speech Tagset. http://arxiv.org/abs/1104.2086
NULL

