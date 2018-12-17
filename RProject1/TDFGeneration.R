#install.packages("tm")#install.packages('rvest')
## The easiest way to get dplyr is to install the whole tidyverse:
#install.packages("tidyverse")
## Alternatively, install just dplyr:
#install.packages("dplyr")
## Or the development version from GitHub:
#install.packages("devtools")
#devtools::install_github("tidyverse/dplyr")
#install.packages("tidyr")
#install.packages(c('devtools', 'curl'))
#devtools::install_github('christophergandrud/DataCombine')
#install.packages("RCurl")
#install.packages("XML")
#install.packages("htm2txt")
#install.packages("SnowballC")
#Loading the rvest package
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

#csvFileName <- 'C:\\Users\\sharath.guduru\\source\\repos\\RProject1\\RProject1\\CorpusExcelra.csv'

csvFileName <- "D:\\R\\test1.csv"

df1 <- read.csv(csvFileName, na.strings = "")

#unique(df1$cleanUrlData)

df2 <- data.frame()

#dtm = DocumentTermMatrix(df1,
                         #control = list(
                                        #stopwords = TRUE,
                                        #wordLengths = c(4, 15),
                                        #removePunctuation = T,
                                        #removeNumbers = T,
 ##stemming = T,
                                        #bounds = list(global = c(minTermFreq, maxTermFreq))

                                        #))



#write.csv((as.matrix(dtm)), "test.csv")

#dtm.matrix = as.matrix(dtm)


for (item in df1) {
    urlValue <- df1[item, "singleUrl"]
    newdata <- df1[item, "cleanUrlData"]
    #print(df1[item, "cleanUrlData"])

    mydata <- Corpus(VectorSource(newdata))

    mydata <- tm_map(mydata, content_transformer(tolower))

    mydata <- tm_map(mydata, content_transformer(gsub), pattern = "\\W", replace = " ")

    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeURL))

    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeNumPunct))

    mydata <- tm_map(mydata, removeWords, stopwords("english"))

    mydata <- tm_map(mydata, stripWhitespace)

    mydata <- tm_map(mydata, removeNumbers)

    mydata <- tm_map(mydata, removePunctuation)

    mydata <- tm_map(mydata, stemDocument)

    dtm <- TermDocumentMatrix(mydata)

    dtmMatrix <- as.matrix(dtm)
    colnames(dtmMatrix) <-  df1[item, "singleUrl"]

    #View(dtmMatrix)

    #write.csv(dtmMatrix, "test.csv")
    #print(dtmMatrix["abhilasha",])

    #print(as.matrix(dtm))
    #write.csv((as.matrix(dtm)), "test.csv")

    #dtm.matrix = as.matrix(dtm)

    #print(mydata)

    #dtm <- TermDocumentMatrix(mydata)
    
    #urlDataFrame = data.frame(urlValue, dtm)
    
    #df2 = rbind(urlDataFrame)

    #df2 = rbind(dtm)
    #print(df2)
}

searchText <- "chemistri"

isSearchTextExists <- dtmMatrix[searchText,]
colnameByOrder <- colnames(dtmMatrix)
df5 <- data.frame(isSearchTextExists, colnameByOrder)
df5
#df5[df5$isSearchTextExists > '0']
df6 <- df5[(df5$isSearchTextExists != 0),]
df7 <- df6[order(df6$isSearchTextExists),]
df7


#searchText
#for (item in searchText) {
    ##if (item == 0)
    ##print(item.)
#}

#View(dtmMatrix)