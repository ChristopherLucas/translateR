library(RCurl)
library(RJSONIO)
library(stringdist)

print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(to.translate, source.lang, target.lang, key){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', key, sep = '')
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')
    api.url <- paste(base, key.str, query, source.str, target.str, sep = '')

    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    translated <- unname(translated)

    out <- list(translated.text = translated, source.text = to.translate,
                source.lang = source.lang, target.lang = target.lang, key = key)
    class(out) <- 'translateClass'
    return(out)
}

rtt <- function(t.obj){
    original.text <- tolower(t.obj$source.text)
    print(original.text)
    translated.back <- tolower(translate(t.obj$translated.text, t.obj$target.lang,
                                         t.obj$source.lang, t.obj$key)$translated.text)
    lv.dist <- stringdist(original.text, translated.back, 'lv')
    return(lv.dist)
}
