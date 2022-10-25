translate <-
function(dataset = NULL, content.field = NULL, content.vec = NULL,
                      google.api.key = NULL, microsoft.api.key = NULL, microsoft.api.region = NULL,
                      source.lang = NULL, target.lang = NULL, microsoft.token=FALSE){

    # Do some sanity checking
    translator <- validateInput(dataset, content.field, content.vec, google.api.key,
                                microsoft.api.key,
                                source.lang, target.lang, microsoft.token)
    
    # Get translation vector
    if(!(is.null(dataset))){
        to.translate <- dataset[[content.field]]
    }
    if(!(is.null(content.vec))){
        to.translate <- content.vec
    }
    
    # Do the translation
    if(translator == 'Google'){
        translated <- unname(
            unlist(
                mclapply(to.translate, function(x) googleTranslate(x, google.api.key, source.lang, target.lang))
                )
            )
    }

    #for longer translation, recommend token method
    if(translator == 'Microsoft' & microsoft.token){
      ptm <- proc.time()
      access.token <- getAccessToken(microsoft.api.key)
	  translated <- c()
      for(doc in to.translate){
        translated <- c(translated, microsoftTranslateToken(doc, access.token, source.lang, target.lang))
        if((proc.time() - ptm)[3] > 540){
          ptm <- proc.time()
          access.token <- getAccessToken(microsoft.api.key)
        }
      }
    
    }else if(translator == 'Microsoft' & !microsoft.token){ 
      #without token method
        translated <- unname(
          unlist(
            mclapply(to.translate, function(x) microsoftTranslate(x, microsoft.api.key, microsoft.api.region, source.lang, target.lang))
          )
        )
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
