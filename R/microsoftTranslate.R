# See documentation at 
# https://docs.microsoft.com/en-us/azure/cognitive-services/translator/reference/v3-0-translate
microsoftTranslate <-
function(x, api.key, source.lang = NULL, target.lang){
    checkText(x)
    microsoft_langs <- languageCodes()$Microsoft
    if(!is.null(source.lang)) {
      source.lang <- match.arg(source.lang, microsoft_langs)
    }
    target.lang <- match.arg(target.lang, microsoft_langs)
    
    base_url <- "https://api.cognitive.microsofttranslator.com/translate"
    body <- data.frame(Text = x)
    query = list(
      "api-version" = "3.0",
      to = target.lang,
      from = source.lang
    )
    response <- POST(
      base_url, 
      body = body,
      add_headers(
        "Ocp-Apim-Subscription-Key" = api.key,
        "Content-Type" = "application/json; charset=UTF-8"
      ),
      query = query,
      encode = "json"
    )
    httr::stop_for_status(response)
    contents <- httr::content(response)
    # This is cleaner with purrr, but extra dependency is overkill for 1 line
    # translations <- map_chr(contents, pluck, "translations", 1, "text")
    translations <- vapply(
      contents,
      function(x) x[["translations"]][[1]][["text"]],
      character(1L)
    )
    return(translations)
    contents

}

checkText <- function(x) {
  if(length(x) > 100) {
    stop("A maximum of 100 strings can be translated at once.")
  }
  if(sum(nchar(x)) > 10000) {
    stop("A maixmum of 10000 characters can be translated at once.")
  }
}
