print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(to.translate, source.lang, target.lang, key, token = FALSE){
    if(token == FALSE){out <- translateText(to.translate, source.lang, target.lang, key)}
    else{out <- translateToken(to.translate, source.lang, target.lang, key)}
    class(out) <- 'translateClass'
    return(out)
}

translateText <- function(to.translate, source.lang, target.lang, key){
    translated <- gTranslate(to.translate, source.lang, target.lang, key)
    out <- list(translated.text = translated, source.text = to.translate,
                source.lang = source.lang, target.lang = target.lang, key = key)
    return(out)
}

translateToken <- function(to.translate, source.lang, target.lang, key){
    translated <- lapply(tokenize(to.translate, source.lang), function(x)
                         gTranslate(x, source.lang, target.lang, key))
    translated <- paste(unlist(translated), collapse=' ')
    out <- list(translated.text = translated, source.text = to.translate,
                source.lang = source.lang, target.lang = target.lang, key = key)
    return(out)
}

tokenize <- function(string, source.lang){
    if(source.lang == 'zh-CN'){return(mmseg4j(string))}
    else{return(MC_tokenizer(string))}
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
