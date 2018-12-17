#Google Authentication

#http://www.ryanpraski.com/google-search-console-api-r-guide-to-get-started/

getGoogleLinks <- function(google.url) {
    doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                             (2.10.0)"))
    html <- htmlTreeParse(doc, useInternalNodes = TRUE, error = function
                          (...) { })
    nodes <- getNodeSet(html, "//h3[@class='r']//a")
    return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

getGoogleLinks("https://www.cancer.org")

