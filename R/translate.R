# Class for objects returned by translate()
print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(dataset = NULL, content.field = NULL, content.vec = NULL,
                      api.key = NULL, translator = 'Google', source.lang, target.lang){

    # Do some sanity checking
    if(is.character(api.key) == FALSE){
        stop("api.key must be a character")
    }
    if((is.null(dataset) | is.null(content.field)) & is.null(content.vec)){
        stop("You must either provide both a dataset and the content field or a single vector of content.")
    }
    if(!(translator %in% c('Google', 'Microsoft'))){
        stop("You must select either 'Google' or 'Microsoft' as the translator")
    }

    

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
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')

    api.url <- paste(base, key.str, query, source.str, target.str, sep = '')
    
    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    return(translated)
}
