#install.packages("DT")
#install.packages("yaml")
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

#source("libraryCustom.R")

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

str_trim <- function(string, side = c("both", "left", "right")) {
    side <- match.arg(side)

    switch(side,
    left = stri_trim_left(string),
    right = stri_trim_right(string),
    both = stri_trim_both(string)
  )
}

str_squish <- function(string) {
    stri_trim_both(str_replace_all(string, "\\s+", " "))
}

str_remove <- function(string, pattern) {
    str_replace(string, pattern, "")
}
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



#Specifying the url for desired website to be scraped
url <- 'https://www.excelra.com/'
shortUrl <- 'excelra.com'
csvFileName <- 'excelramainpagetrim.csv' #'cancermainpagetrim.csv'
csvCorpusFileName <- 'CorpusExcelra.csv'

getLinks <- function(url, shortUrl, csvFileName,session) {
    webpage <- read_html(url)

    glinks <- scraplinks(url)

    cleanupUrls <- unique(str_trim(glinks$url, "both"))
    write.csv(cleanupUrls, csvFileName)
   
}

getCorpus <- function(url, shortUrl, csvFileName,session) {
    df2 <- read.csv(csvFileName, na.strings = "") #cleanupUrls
    df2 <- df2[!(df2$x == "#"),]
    df3 <- df2[!(df2$x == "/"),]
    #df4 <- df3 %>% filter(x != "NA")    -- Very imp function
    df4 <- df3 %>% filter(x != "NA")
    df4$x <- gsub('^/', url, df4$x)
    df4$x <- gsub('^#', paste(url, '#'), df4$x)
    df5 <- dplyr::filter(df4, grepl(shortUrl, x)) #Need to make domain name dynamic
    df6 <- dplyr::filter(df5, !grepl('tel:', x))
    df6 <- dplyr::filter(df5, !grepl('mailto:', x))
    #df6 <- dplyr::filter(df5, !grepl('https:', x))

    allUrls <- df6['x']
    completeUrlsText <- data.frame()
    for (urlOfPage in allUrls$x) {
    singleUrl <- ""
    singleUrl <- str_replace_all(urlOfPage, fixed(" "), "")
    urltextData <- clean_link(singleUrl)
    cleanUrlData <- cleanStrings(urltextData)
    urlAndText <- data.frame(singleUrl, cleanUrlData)
    completeUrlsText <- rbind(completeUrlsText, urlAndText)
    }

    #View(completeUrlsText)
    write.csv(completeUrlsText, paste("D:\\R\\CorpusUpdate.csv")) #, shortUrl, format(session$userData$time(), format = "%m/%d/%Y %H:%M"), ".csv"))
}


ui <- fluidPage(  
  textInput("txtUrl", "UrlName",placeholder = "https://www.excelra.com/"),
  textOutput("entered_txtUrl"),

  textInput("txtShortUrl", "Short Url",placeholder ='excelra.com' ),
  textOutput("entered_txtShortUrl"),

  textInput("txtFileName", "Output file name with location"),
  textOutput("entered_txtFileName"),

  actionButton("rmv", "Generate Corpus"),

  DT::dataTableOutput("mytable")
)

server <- function(input, output, session) {
    output$entered_txtUrl <- renderText({
    paste("You have entered", input$txtUrl)
    })

    output$entered_txtShortUrl <- renderText({
    paste("You have entered", input$txtShortUrl)
    })

    output$entered_txtFileName <- renderText({
    paste("You have entered", input$txtFileName)
    })

    observeEvent(input$rmv, {
    output$mytable = DT::renderDataTable({
        getLinks(input$txtUrl, input$txtShortUrl, input$txtFileName,session)
        getCorpus(input$txtUrl, input$txtShortUrl, input$txtFileName, session)
        output$entered_txtFileName <- renderText({
            paste("Data generated successfully")
        })
            #emp.data <- data.frame(
            #emp_id = c(1:5),
            #emp_name = c("Rick", "Dan", "Michelle", "Ryan", "Gary"),
            #salary = c(623.3, 515.2, 611.0, 729.0, 843.25),

                #start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
              #"2015-03-27")),
            #stringsAsFactors = FALSE           
            #)
    
    })
    })

}

shinyApp(ui, server)