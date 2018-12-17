#library(httr)
#query = "https://www.googleapis.com/customsearch/v1?key=API_KEY&cx=ENGINE_ID&q=SEARCH_TERM"
#content(GET(query))


#install.packages('jsonlite')
library('jsonlite')
#moddate
#df1 <- getURL("https://www.googleapis.com/customsearch/v1?key=AIzaSyAhre6i0dugGoBEsdTj83WERWU3iDVDNq0&cx=017576662512468239146:omuauf_lfve&q=www.angiochem.com/pipeline", ssl.verifypeer = TRUE)

#df1

fromJSON("https://www.googleapis.com/customsearch/v1?key=AIzaSyAhre6i0dugGoBEsdTj83WERWU3iDVDNq0&cx=017576662512468239146:omuauf_lfve&q=lectures")

#mydata

#data = rjson