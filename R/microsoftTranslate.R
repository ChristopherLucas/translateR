microsoftTranslate <-
function(x, api.key, source.lang, target.lang){
  
    params = paste("text=", URLencode(x), "&to=", target.lang, "&from=", source.lang, sep = '')
    translateUrl = paste("http://api.microsofttranslator.com/v2/Http.svc/Translate?", params, sep = '')
    
    return(
        cleanFun(
            GET(translateUrl, add_headers("Ocp-Apim-Subscription-Key" = api.key))
            )
        )

}
