# Functions to print the language codes for
# the Google and Microsoft Translation APIs

getMicrosoftLanguages <- function(){
    cat("
LANGUAGE                CODE
----------------------------

Arabic:                 'ar'
Bulgarian:              'bg'
Catalan:                'ca'
Chinese (Simplified):   'zh-CHS
Chinese (Traditional):  'zh-CHT
Czech:                  'cs	
Danish:                 'da
Dutch:                  'nl
English:                'en
Estonian:               'et
Finnish:                'fi
French:                 'fr
German:                 'de
Greek:                  'el
Haitian Creole:         'ht
Hebrew:                 'he
Hindi:                  'hi
Hmong Daw:              'mww
Hungarian:              'hu
Indonesian:             'id
Italian:                'it
Japanese:               'ja
Klingon:                'tlh
Klingon (pIqaD):        'tlh-Qaak
Korean:                 'ko
Latvian:                'lv
Lithuanian:             'lt
Malay:                  'ms
Maltese:                'mt
Norwegian:              'no
Persian:                'fa
Polish:                 'pl
Portuguese:             'pt
Romanian:               'ro
Russian:                'ru
Slovak:                 'sk
Slovenian:              'sl'
Spanish:                'es'
Swedish:                'sv'
Thai:                   'th'
Turkish:                'tr'
Ukrainian:              'uk'
Urdu:                   'ur'
Vietnamese:             'vi'
Welsh:                  'cy'
")
}

getGoogleLanguages <- function(){
    cat("
LANGUAGE                CODE
----------------------------

Afrikaans:              'af'
Albanian:               'sq'
Arabic:                 'ar'
Armenian:               'hy'
Azerbaijani:            'az'
Basque:                 'eu'
Belarusian:             'be'
Bengali:                'bn'
Bosnian:                'bs'
Bulgarian:              'bg'
Catalan:                'ca'
Cebuano:                'ceb'
Chinese (Simplified):   'zh-CN'
Chinese (Traditional):  'zh-TW'
Croatian:               'hr'
Czech:                  'cs'
Danish:                 'da'
Dutch:                  'nl'
English:                'en'
Esperanto:              'eo'
Estonian:               'et'
Filipino:               'tl'
Finnish:                'fi'
French:                 'fr'
Galician:               'gl'
Georgian:               'ka'
German:                 'de'
Greek:                  'el'
Gujarati:               'gu'
Haitian Creole:         'ht'   
Hausa:                  'ha'
Hebrew:                 'iw'
Hindi:                  'hi'
Hmong:                  'hmn'
Hungarian:              'hu'
Icelandic:              'is'
Igbo:                   'ig'
Indonesian:             'id'
Irish:                  'ga'
Italian:                'it'
Japanese:               'ja'
Javanese:               'jw'
Kannada:                'kn'
Khmer:                  'km'
Korean:                 'ko'
Lao:                    'lo'
Latin:                  'la'
Latvian:                'lv'
Lithuanian:             'lt'
Macedonian:             'mk'
Malay:                  'ms'
Maltese:                'mt'
Maori:                  'mi'
Marathi:                'mr'
Mongolian:              'mn'
Nepali:                 'ne'
Norwegian:              'no'
Persian:                'fa'
Polish:                 'pl'
Portuguese:             'pt'
Punjabi:                'pa'
Romanian:               'ro'
Russian:                'ru'
Serbian:                'sr'
Slovak:                 'sk'
Slovenian:              'sl'
Somali:                 'so'
Spanish:                'es'
Swahili:                'sw'
Swedish:                'sv'
Tamil:                  'ta'
Telugu:                 'te'
Thai:                   'th'
Turkish:                'tr'
Ukrainian:              'uk'
Urdu:                   'ur'
Vietnamese:             'vi'
Welsh:                  'cy'
Yiddish:                'yi'
Yoruba:                 'yo'
Zulu:                   'zu'
")
}
