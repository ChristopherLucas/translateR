# Generate the list of languages supported by Microsoft Translator.
# See docs at
# https://docs.microsoft.com/en-us/azure/cognitive-services/translator/reference/v3-0-languages
# You can do this dynamically, but since it shouldn't change often, it seems 
# more efficient to store the values.
# They are given in languageCodes()$Microsoft

library(httr)
library(purrr)

response <- GET("https://api.cognitive.microsofttranslator.com/languages?api-version=3.0&scope=translation")
lang_data <- content(response)[[1]]

lang_names <- lang_data %>% 
  unname() %>% 
  map_chr(~ chuck(.x, "name"))

langs <- lang_data %>% 
  names() %>% 
  as.list() %>% 
  set_names(lang_names)
