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


validateInput <- function(dataset, content.field, content.vec, api.key, translator, source.lang, target.lang){
    if(is.character(api.key) == FALSE){
        stop("api.key must be a character type.")
    }
    if((is.null(dataset) | is.null(content.field)) & is.null(content.vec)){
        stop("You must either provide both a dataset and the content field or a single vector of content.")
    }
    if(!(translator %in% c('Google', 'Microsoft'))){
        stop("You must select either 'Google' or 'Microsoft' as the translator.")
    }
    if(translator == 'Google'){
        if(!(source.lang %in% unname(unlist(languages['Google'])))){
            msg <- paste("The source.lang '", source.lang, "' is not a valid Google language code. To see a list of Google language codes, use getGoogleLanguages().", sep = '')
            stop(msg)
        }
        if(!(target.lang %in% unname(unlist(languages['Google'])))){
            msg <- paste("The target.lang '", target.lang, "' is not a valid Google language code. To see a list of Google language codes, use getGoogleLanguages().", sep = '')
            stop(msg)
        }
    }    
    if(translator == 'Microsoft'){
        if(!(source.lang %in% unname(unlist(languages['Microsoft'])))){
            msg <- paste("The source.lang '", source.lang, "' is not a valid Microsoft language code. To see a list of Microsoft language codes, use getMicrosoftLanguages().", sep = '')
            stop(msg)
        }
        if(!(target.lang %in% unname(unlist(languages['Microsoft'])))){
            msg <- paste("The target.lang '", target.lang, "' is not a valid Microsoft language code. To see a list of Microsoft language codes, use getMicrosoftLanguages().", sep = '')
            stop(msg)
        }
    }
    if(!(is.null(dataset))){
        if(!(is.data.frame(dataset))){stop("dataset must be a data.frame.")}
    }
    if(!(is.null(content.field))){
        if(!(is.character(content.field))){stop("content.field must be a character.")}
    }
    if(!(is.null(content.field))){
        if(!(is.character(dataset[[content.field]]))){stop("The column containing the content must be a character vector.")}
    }
    if(!(is.null(content.vec))){
        if(!(is.character(content.vec))){stop("content.vec must be a character vector.")}
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
