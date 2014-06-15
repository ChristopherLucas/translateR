CLTM
====

R Package for Cross-Language Topic Modeling

INSTALLATION
-
library(devtools)

install_github('translateR', username = 'ChristopherLucas')


translate
-

translate(to.translate, source.lang, target.lang, key, token = FALSE)

to.translate: string of the text that is to be translated

source.lang: the google code of the source language (for example, 'zh-CN' for chinese)

target.lang: the google code for the target langage ('en' for english)

key: your google API key

token: if true, text is tokenized before translation. If false, text is tokenized after translation

rtt
-

A measure of how bad round-trip-translation is. Takes an object created by translate() and returns the Levenshtein distance between the original text and the text after round-trip-translation.
