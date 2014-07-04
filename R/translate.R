translate <-
function(dataset = NULL, content.field = NULL, content.vec = NULL,
                      api.key = NULL, client.id = NULL, client.secret = NULL,
                      translator = 'Google', source.lang = NULL, target.lang = NULL){

    # Do some sanity checking
    validateInput(dataset, content.field, content.vec, api.key, client.id, client.secret,
                          translator, source.lang, target.lang)

    # Get translation vector
    if(!(is.null(dataset))){
        to.translate <- dataset[[content.field]]
    }
    if(!(is.null(content.vec))){
        to.translate <- content.vec
    }

    checkLang(to.translate, source.lang, translator)
    
    # Do the translation
    if(translator == 'Google'){
        translated <- unname(
            unlist(
                mclapply(to.translate, function(x) googleTranslate(x, api.key, source.lang, target.lang))
                )
            )
    }

    if(translator == 'Microsoft'){ 
        ptm <- proc.time()
        access.token <- getAccessToken(client.id, client.secret)
        translated <- c()
        for(doc in to.translate){
            translated <- c(translated, microsoftTranslate(doc, access.token, source.lang, target.lang))
            if((proc.time() - ptm)[3] > 540){
                ptm <- proc.time()
                access.token <- getAccessToken(client.id, client.secret)
            }
        }
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
