library(RCurl)
library(RJSONIO)

translate <- function(to.translate, source.lang, target.lang, key){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key <- paste('key=', key, sep = '')
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')
    api.url <- paste(base, key, query, source.str, target.str, sep = '')

    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    translated <- unname(translated)
    return(translated)
}

