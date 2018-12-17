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


csvFileName <- "D:\\R\\test1.csv"

df1 <- read.csv(csvFileName, na.strings = "")
df2 <- data.frame()

for (item in df1) {
    urlValue <- df1[item, "singleUrl"]
    newdata <- df1[item, "cleanUrlData"]
    #View(newdata)
    mydata <- VCorpus(VectorSource(newdata))

    mydata <- tm_map(mydata, content_transformer(tolower))

    myda.ta <- tm_map(mydata, content_transformer(gsub), pattern = "\\W", replace = " ")

    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeURL))

    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeNumPunct))

    mydata <- tm_map(mydata, removeWords, stopwords("english"))

    mydata <- tm_map(mydata, stripWhitespace)

    mydata <- tm_map(mydata, removeNumbers)

    mydata <- tm_map(mydata, removePunctuation)
    #View(mydata)

    #mydata <- tm_map(mydata, stemDocument)

    dtm <- TermDocumentMatrix(mydata)

    dtmMatrix <- as.matrix(dtm)
    colnames(dtmMatrix) <- df1[item, "singleUrl"]

    #View(dtmMatrix)
}

rownamesByOrder <- rownames(dtmMatrix)

ui <- basicPage(
navbarPage("My Application",
           tabPanel("Search by word", uiOutput('page1'))
           #,
           #tabPanel("Data", uiOutput('page2'))
           ),

h2("Search Result"),

 # Input: Selector for choosing dataset ----
 selectInput(inputId = "var",
                  label = "Choose a keyword:",
                  choices = rownamesByOrder),
  textOutput("selected_var"),
  DT::dataTableOutput("mytable"),
  DT::dataTableOutput("mytable1")
)

server <- function(input, output,session) {
    output$page1 <- renderUI({
                            output$selected_var <- renderText({
                                paste("You have selected",input$var)
                            })

                            output$mytable = DT::renderDataTable({
                            #df7
                            isSearchTextExists <- dtmMatrix[input$var,]
                            colnameByOrder <- colnames(dtmMatrix)
                            df5 <- data.frame(isSearchTextExists, colnameByOrder)
                            df5
                            df6 <- df5[(df5$isSearchTextExists != 0),]
                            
                            df7 <- df6[order(df6$isSearchTextExists),]
                            df7

                            #df8 <- nrow(distinct(df6, df6$colnameByOrder))
                            #df8
                            #df8 <- df7 %>% dplyr::distinct(,,df7$colnameByOrder)
                            #df8
                            })

                            output$mytable1 = DT::renderDataTable({
                             dtmMatrix
                            })
    })

    #output$page2 <- renderUI({
    #})
}

shinyApp(ui, server)

