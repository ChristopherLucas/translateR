validateInput <-
function(dataset, content.field, content.vec, api.key, client.id, client.secret,
                          translator, source.lang, target.lang){

    if(translator == 'Google'){
        if(is.character(api.key) == FALSE){
            stop("You must provide a valid api.key as character to use the Google translator.")
        }
    }
    if(translator == 'Microsoft'){
        if(is.character(client.id) == FALSE){
            stop("You must provide a valid client.id as character to use the Microsoft translator.")
        }
        if(is.character(client.secret) == FALSE){
            stop("You must provide a valid client.secret as character to use the Microsoft translator.")
        }
        
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
