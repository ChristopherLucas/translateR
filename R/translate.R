print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(to.translate, source.lang, target.lang, key){
    out <- translateText(to.translate, source.lang, target.lang, key)
    class(out) <- 'translateClass'
    return(out)
}

translateText <- function(to.translate, source.lang, target.lang, key){
    to.translate <- combine(to.translate)
    translated <- gTranslate(to.translate, source.lang, target.lang, key)
    translated <- splitTranslated(translated)
    out <- list(translated.text = translated, source.text = to.translate,
                source.lang = source.lang, target.lang = target.lang, key = key)
    return(out)
}

combine <- function(to.translate){
    to.translate <- paste(to.translate, collapse = " ----12345-54321---- ")
    return(to.translate)
}

splitTranslated <- function(translated){
    translated <- unlist(strsplit(translated, '---- 12345-54321 ----'))
    translated <- unlist(lapply(translated, function(x) trim(x)))
    return(translated)
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gTranslate <- function(to.translate, source.lang, target.lang, key){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', key, sep = '')
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')
    api.url <- paste(base, key.str, query, source.str, target.str, sep = '')

    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    translated <- unname(translated)
    return(translated)
}

rtt <- function(t.obj){
    original.text <- tolower(t.obj$source.text)
    translated.back <- tolower(translate(t.obj$translated.text, t.obj$target.lang,
                                         t.obj$source.lang, t.obj$key)$translated.text)
    lv.dist <- stringdist(original.text, translated.back, 'lv')
    return(lv.dist)
}
