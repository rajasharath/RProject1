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

source('C:\\Users\\sharath.guduru\\source\\repos\\RProject1\\RProject1\\libraryCustom.R')

#1
getLinksFromHtmlPage <- function(urlInfo) {
    print("1 :  1")
    glinks <- scraplinks(urlInfo)
    print("1 :  2")
    return(glinks)
}

#2
trimUrlsCustom <- function(urlsList) {
    print("2 :  1")
    cleanupUrls <- unique(str_trim(glinks$url, "both"))
    print("2 :  2")
    return(cleanupUrls)
}

#3
cleanAndFilterUrlsCustom <- function(urlsList, shortUrl) {
    print("3 :  1")
    csvData <- read.csv(csvFileName, na.strings = "")
    print(csvData)
    print("3 :  2")
    csvData <- csvData[!(csvData$x == "#"),]
    print(csvData)
    print("3 :  3")
    csvDataFormatted <- csvData[!(csvData$x == "/"),]
    print(csvDataFormatted)  
    print("3 :  4")
    dataWithoutNAs <- csvDataFormatted %>% filter(x != "NA") #df4 <- df3 %>% filter(x != "NA")    -- Very imp function
    print(dataWithoutNAs)
    print("3 :  5")
    dataWithoutNAs$x <- gsub('^/', url, dataWithoutNAs$x)
    print(dataWithoutNAs$x)
    print("3 :  6")
    dataWithoutNAs$x <- gsub('^#', paste(url, '#'), dataWithoutNAs$x)
    print(dataWithoutNAs$x)
    print("3 :  7")
    replacingShortDomainName <- dplyr::filter(dataWithoutNAs, grepl(shortUrl, x)) #Need to make domain name dynamic
    print(replacingShortDomainName)
    print("3 :  8")
    removeTelephoneNumber <- dplyr::filter(replacingShortDomainName, !grepl('tel:', x))
    print(removeTelephoneNumber)
    print("3 :  9")
    removeMailToAddress <- dplyr::filter(replacingShortDomainName, !grepl('mailto:', x))
    print(removeMailToAddress)
    print("3 :  10")
    print("end of method cleanAndFilterUrlsCustom")
    #df6 <- dplyr::filter(df5, !grepl('https:', x))
    return(removeMailToAddress['x'])
}

#4
buildCorpus <- function(urlsList) {
   print("4 :  1")
    completeUrlsText <- data.frame()
    print('empty data frame')
    print(completeUrlsText)
   print("4 :  2")
   for (urlOfPage in urlsList$x) {
    singleUrl <- ""
    singleUrl <- str_replace_all(urlOfPage, fixed(" "), "")
    urltextData <- clean_link(singleUrl)
    cleanUrlData <- cleanStrings(urltextData)
    urlAndText <- data.frame(singleUrl, cleanUrlData)
    completeUrlsText <- rbind(completeUrlsText, urlAndText)
   }
    print("4 :  3")
    print(completeUrlsText)
    print("end of method buildCorpus")
   return(completeUrlsText)
}

#Specifying the url for desired website to be scraped
#url <- 'https://www.cancer.org/'
#shortUrl <- 'cancer.org'
#csvFileName <- 'cancermainpagetrim.csv' #'cancermainpagetrim.csv'
#csvCorpusFileName <- 'CorpusCancer.csv'


#5
getCorpus <- function(urlInfo, shortUrl, fileName) {
    print(urlInfo)
    df1 <- getLinksFromHtmlPage(urlInfo)
    print(df1)
    df2 <- trimUrlsCustom(df1)
    print(df2)
    df3 <- cleanAndFilterUrlsCustom(df2, shortUrl)
    print(df3)
    df4 <- buildCorpus(df3)
    print(df4)        
    return(df4)
}

resultData <- getCorpus('https://www.cancer.org/', 'cancer.org', 'CorpusCancer.csv')
print('final result')
resultData

#viewCorpus <- function(urlInfo, shortUrl, fileName) {
    #result = getCorpus(urlInfo, shortUrl, fileName)
    #return(View(result))
#}


#createCorpus <- function(urlInfo, shortUrl, fileName) {
    #result = getCorpus(urlInfo, shortUrl, fileName)
    #write.csv(result, csvFileName)
#}



