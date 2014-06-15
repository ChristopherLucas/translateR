print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(to.translate, source.lang, target.lang, key){
    out <- translateText(to.translate, source.lang, target.lang, key)
    class(out) <- 'translateClass'
    return(out)

}
translateText <- function(to.translate, source.lang, target.lang, key){
    to.translate.original <- to.translate
    translated <- gTranslate(to.translate, source.lang, target.lang, key)
    out <- list(translated.text = translated, source.text = to.translate.original,
                source.lang = source.lang, target.lang = target.lang, key = key)
    return(out)
}

gTranslate <- function(to.translate, source.lang, target.lang, key){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', key, sep = '')
    query <- curlEscape(to.translate)
    queries <- querySplit(query)
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')

    api.url <- paste(base, key.str, q, source.str, target.str, sep = '')
    
    translated <- tryCatch(fromJSON(getURL(api.url))$data$translations[[1]], error = function(e) e)
    return(translated)
}
 
rtt <- function(t.obj){
    original.text <- tolower(t.obj$source.text)
    translated.back <- tolower(translate(t.obj$translated.text, t.obj$target.lang,
                                         t.obj$source.lang, t.obj$key)$translated.text)
    lv.dist <- stringdist(original.text, translated.back, 'lv')
    return(lv.dist)
}
