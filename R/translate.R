# Class for objects returned by translate()
print.translateClass <- function(x){
    print(x$translated.text)
}

getGoogleLanguages <- function(){
    cat("    LANGUAGE:
    ----------------------------------
    Afrikaans: 
    Albanian:     
    Arabic: 
    Armenian:
    Azerbaijani:
    Basque:
    Belarusian:
    Bengali:
    Bosnian:
    Bulgarian:
    Catalan:
    Cebuano:
    Chinese:
    Croatian:
    Czech:
    Danish:
    Dutch:
    English:
    Esperanto:
    Estonian:
    Filipino:
    Finnish:
    French:
    Galician:          
    Georgian:          'ka
    German:            'de'
    Greek:             'el'
    Gujarati:          'gu'
    Haitian Creole:    'ht'   
    Hausa:
    Hebrew:
    Hindi:
    Hmong:
    Hungarian:
    Icelandic:
    Igbo:
    Indonesian:
    Irish:
    Italian:
    Japanese:
    Javanese:
    Kannada:
    Khmer:
    Korean:
    Lao:
    Latin:
    Latvian:
    Lithuanian:
    Macedonian:
    Malay:
    Maltese:
    Maori:
    Marathi:
    Mongolian:
    Nepali:
    Norwegian:
    Persian:
    Polish:
    Portuguese:
    Punjabi:
    Romanian:
    Russian:
    Serbian:
    Slovak:
    Slovenian:
    Somali:
    Spanish:
    Swahili:
    Swedish:
    Tamil:
    Telugu:
    Thai:
    Turkish:
    Ukrainian:
    Urdu:
    Vietnamese:
    Welsh:
    Yiddish:
    Yoruba:
    Zulu:
")
}

translate <- function(dataset = NULL, content.field = NULL, content.vec = NULL,
                      api.key, translator = 'Google', source.lang, target.lang){
    
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
