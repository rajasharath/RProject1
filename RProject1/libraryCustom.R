library(shiny)
library(ggplot2) # for the diamonds dataset

library('rvest')
library('stringi')
library('stringr')
library('tidyverse')
library('dplyr')
library('tidyr')
library('devtools')
library('RCurl')
library('XML')
library('tm')
library('SnowballC')
library('yaml')
library('DT')



#' Extract link texts and urls from a web page
#' @param url character an url
#' @return a data frame of link text and urls
#' @examples
#' \dontrun{
#' scraplinks("http://localhost/")
#' glinks <- scraplinks("http://google.com/")
#' }
#' @export
scraplinks <- function(url) {
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    # Extract the link text
    link_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    data_frame = data.frame(link = link_, url = url_)
    return(data_frame)
}
#' Trim whitespace from a string
#'
#' `str_trim()` removes whitespace from start and end of string; `str_squish()`
#' also reduces repeated whitespace inside a string.
#'
#' @param string A character vector.
#' @param side Side on which to remove whitespace (left, right or both).
#' @return A character vector.
#' @export
#' @seealso [str_pad()] to add whitespace
#' @examples
#' str_trim("  String with trailing and leading white space\t")
#' str_trim("\n\nString with trailing and leading white space\n\n")
#'
#' str_squish("  String with trailing,  middle, and leading white space\t")
#' str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
str_trim <- function(string, side = c("both", "left", "right")) {
    side <- match.arg(side)

    switch(side,
    left = stri_trim_left(string),
    right = stri_trim_right(string),
    both = stri_trim_both(string)
  )
}

#' @export
#' @rdname str_trim
str_squish <- function(string) {
    stri_trim_both(str_replace_all(string, "\\s+", " "))
}


str_remove <- function(string, pattern) {
    str_replace(string, pattern, "")
}

#' @export
#' @rdname str_remove
str_remove_all <- function(string, pattern) {
    str_replace_all(string, pattern, "")
}

clean_link <- function(link) {
    html = getURL(link, followlocation = TRUE)

    # parse html
    doc = htmlParse(html, asText = TRUE)
    plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    text <- paste(plain.text, collapse = "\n")
    return(text)
}

cleanStrings <- function(s) {
    # first fix the unicode ' characters to be all consistent
    s <- gsub("\xe2\x80\x99", "'", s, perl = TRUE)
    s <- gsub("\u0091|\u0092|\u0093|\u0094|\u0060|\u0027|\u2019|\u000A", "'", s, perl = TRUE)

    # Strip unwanted UTF-8 characters
    s <- iconv(s, "UTF-8", "ASCII", "?")
    # strip unused characters but leave ' and -
    # s <- gsub("[^[:alpha:][:space:]'-]", " ", s)

    # now let's get rid of single quotes that are quoted strings and not in the middle of a word
    # this will leave contractions like don't, I'm, etc.
    s <- gsub("(?<!\\w)[-'](?<!\\w)", " ", s, perl = TRUE) # just leave - and ' in the middle of words

    s <- gsub("[[:space:]]+", " ", s, perl = TRUE) # consolidate spaces
    s <- gsub("^[[:space:]]+", "", s, perl = TRUE) # strip leading spaces
    s <- tolower(s)

    return(s)
}