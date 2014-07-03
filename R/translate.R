translate <- function(dataset = NULL, content.field = NULL, content.vec = NULL,
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

getAccessToken <- function(client.id, client.secret){
    fields <- list(
        client_id = client.id,
        client_secret = client.secret,
        scope = 'http://api.microsofttranslator.com',
        grant_type = 'client_credentials'
        )

    return(
        fromJSON(postForm('https://datamarket.accesscontrol.windows.net/v2/OAuth2-13',
                          .params = fields,
                          style = 'POST'))[['access_token']]
        )
}
    
microsoftTranslate <- function(x, access.token, source.lang, target.lang){
    params = paste("text=", URLencode(x), "&to=", target.lang, "&from=", source.lang, sep = '')
    translateUrl = paste("http://api.microsofttranslator.com/v2/Http.svc/Translate?", params, sep = '')
    
    return(
        cleanFun(
            GET(translateUrl, add_headers(Authorization = paste('Bearer', access.token)))
            )
        )
}

googleTranslate <- function(x, api.key, source.lang, target.lang){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', api.key, sep = '')
    query <- paste('&q=', curlEscape(x), sep = '')
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')
    
    api.url <- paste(base, key.str, query, source.str, target.str, sep = '')
 
    translated <- fromJSON(getURL(api.url))$data$translations[[1]]
    return(translated)
}

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}




    

   
 
