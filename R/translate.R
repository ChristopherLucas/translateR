translate <- function(dataset = NULL, content.field = NULL, content.vec = NULL,
                      api.key = NULL, translator = 'Google', source.lang = NULL, target.lang = NULL){

    # Do some sanity checking
    validateInput(dataset, content.field, content.vec, api.key, translator, source.lang, target.lang)

    # Get translation vector
    if(!(is.null(dataset))){
        to.translate <- dataset[[content.field]]
    }
    if(!(is.null(content.vec))){
        to.translate <- content.vec
    }

    # Do the translation
    if(translator == 'Google'){
        translated <- googleTranslate(to.translate, api.key, source.lang, target.lang)
    }

    # Figure out what we should return
    if(!(is.null(content.vec))){
        return(translated)
    }
    if(!(is.null(dataset) & is.null(content.field))){
        dataset$translatedContent <- translated
        return(dataset)
    }
}


googleTranslate <- function(to.translate, api.key, source.lang, target.lang){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', api.key, sep = '')
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')

    api.url <- paste(base, key.str, query, source.str, target.str, sep = '')
    
    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    return(translated)
}
