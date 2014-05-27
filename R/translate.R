print.translateClass <- function(x){
    print(x$translated.text)
}

translate <- function(to.translate, source.lang, target.lang, key){
    out <- translateText(to.translate, source.lang, target.lang, key)
    class(out) <- 'translateClass'
    return(out)
}

translateText <- function(to.translate, source.lang, target.lang, key){
    to.translate.original <- to.translate
    to.translate <- combine(to.translate)
    translated <- gTranslate(to.translate, source.lang, target.lang, key)
    translated <- splitTranslated(translated)
    out <- list(translated.text = translated, source.text = to.translate.original,
                source.lang = source.lang, target.lang = target.lang, key = key)
    return(out)
}

combine <- function(to.translate){
    to.translate <- paste(to.translate, collapse = " ----1234554321---- ")
    return(to.translate)
}

splitTranslated <- function(translated){
    translated <- unlist(strsplit(translated, '---- 1234554321 ----'))
    translated <- unlist(lapply(translated, function(x) trim(x)))
    return(translated)
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

strdehtml <- function(s){
    ret <- gsubfn("&#([0-9]+);", function(x) rawToChar(as.raw(as.numeric(x))), s)
    ret <- gsubfn("&([^;]+);", function(x) htmlchars[x], ret)
    return(ret)
}

gTranslate <- function(to.translate, source.lang, target.lang, key){
    base <- 'https://www.googleapis.com/language/translate/v2?'
    key.str <- paste('key=', key, sep = '')
    query <- paste('&q=', curlEscape(to.translate), sep = '')
    queries <- querySplit(query)
    source.str <- paste('&source=', source.lang, sep = '')
    target.str <- paste('&target=', target.lang, sep = '')

    translated.out <- c()
    for(query in queries){
        api.url <- paste(base, key.str, query, source.str, target.str, sep = '')

        translated <- fromJSON(getURL(api.url))$data$translations[[1]]
        translated <- unname(strdehtml(translated))
        translated.out <- combine(c(translated.out, translated))
    }
    return(translated.out)
}

querySplit <- function(query){
    string.vec <- c()
    start.and.finish <- str_locate_all(query, '----1234554321----')[[1]]
    prev.end <- 0
    print(start.and.finish)
    print(nchar(query))
    for(i in seq(0, nchar(query), 1000)){
        start.index <- which.min(abs(start.and.finish[,1] - i))
        string.vec <- c(string.vec, substr(query, prev.end, start.and.finish[,1][start.index]))
        prev.end <- start.and.finish[,2][start.index]
    }
    return(string.vec)
}

rtt <- function(t.obj){
    original.text <- tolower(t.obj$source.text)
    translated.back <- tolower(translate(t.obj$translated.text, t.obj$target.lang,
                                         t.obj$source.lang, t.obj$key)$translated.text)
    lv.dist <- stringdist(original.text, translated.back, 'lv')
    return(lv.dist)
}

htmlchars <- setNames(
c(
"\u0022", "\u0026", "\u0027", "\u003C", "\u003E", "\u00A0", "\u00A1", "\u00A2", "\u00A3", "\u00A4", "\u00A5", "\u00A6", "\u00A7", "\u00A8", "\u00A9", "\u00AA", "\u00AB", "\u00AC", "\u00AD", "\u00AE", "\u00AF", "\u00B0", "\u00B1", "\u00B2", "\u00B3", "\u00B4", "\u00B5", "\u00B6", "\u00B7", "\u00B8", "\u00B9", "\u00BA", "\u00BB", "\u00BC", "\u00BD", "\u00BE", "\u00BF", "\u00C0", "\u00C1", "\u00C2", "\u00C3", "\u00C4", "\u00C5", "\u00C6", "\u00C7", "\u00C8", "\u00C9", "\u00CA", "\u00CB", "\u00CC", "\u00CD", "\u00CE", "\u00CF", "\u00D0", "\u00D1", "\u00D2", "\u00D3", "\u00D4", "\u00D5", "\u00D6", "\u00D7", "\u00D8", "\u00D9", "\u00DA", "\u00DB", "\u00DC", "\u00DD", "\u00DE", "\u00DF", "\u00E0", "\u00E1", "\u00E2", "\u00E3", "\u00E4", "\u00E5", "\u00E6", "\u00E7", "\u00E8", "\u00E9", "\u00EA", "\u00EB", "\u00EC", "\u00ED", "\u00EE", "\u00EF", "\u00F0", "\u00F1", "\u00F2", "\u00F3", "\u00F4", "\u00F5", "\u00F6", "\u00F7", "\u00F8", "\u00F9", "\u00FA", "\u00FB", "\u00FC", "\u00FD", "\u00FE", "\u00FF", "\u0152", "\u0153", "\u0160", "\u0161", "\u0178", "\u0192", "\u02C6", "\u02DC", "\u0391", "\u0392", "\u0393", "\u0394", "\u0395", "\u0396", "\u0397", "\u0398", "\u0399", "\u039A", "\u039B", "\u039C", "\u039D", "\u039E", "\u039F", "\u03A0", "\u03A1", "\u03A3", "\u03A4", "\u03A5", "\u03A6", "\u03A7", "\u03A8", "\u03A9", "\u03B1", "\u03B2", "\u03B3", "\u03B4", "\u03B5", "\u03B6", "\u03B7", "\u03B8", "\u03B9", "\u03BA", "\u03BB", "\u03BC", "\u03BD", "\u03BE", "\u03BF", "\u03C0", "\u03C1", "\u03C2", "\u03C3", "\u03C4", "\u03C5", "\u03C6", "\u03C7", "\u03C8", "\u03C9", "\u03D1", "\u03D2", "\u03D6", "\u2002", "\u2003", "\u2009", "\u200C", "\u200D", "\u200E", "\u200F", "\u2013", "\u2014", "\u2018", "\u2019", "\u201A", "\u201C", "\u201D", "\u201E", "\u2020", "\u2021", "\u2022", "\u2026", "\u2030", "\u2032", "\u2033", "\u2039", "\u203A", "\u203E", "\u2044", "\u20AC", "\u2111", "\u2118", "\u211C", "\u2122", "\u2135", "\u2190", "\u2191", "\u2192", "\u2193", "\u2194", "\u21B5", "\u21D0", "\u21D1", "\u21D2", "\u21D3", "\u21D4", "\u2200", "\u2202", "\u2203", "\u2205", "\u2207", "\u2208", "\u2209", "\u220B", "\u220F", "\u2211", "\u2212", "\u2217", "\u221A", "\u221D", "\u221E", "\u2220", "\u2227", "\u2228", "\u2229", "\u222A", "\u222B", "\u2234", "\u223C", "\u2245", "\u2248", "\u2260", "\u2261", "\u2264", "\u2265", "\u2282", "\u2283", "\u2284", "\u2286", "\u2287", "\u2295", "\u2297", "\u22A5", "\u22C5", "\u22EE", "\u2308", "\u2309", "\u230A", "\u230B", "\u2329", "\u232A", "\u25CA", "\u2660", "\u2663", "\u2665"),
c("quot", "amp", "apos", "lt", "gt", "nbsp", "iexcl", "cent", "pound", "curren", "yen", "brvbar", "sect", "uml", "copy", "ordf", "laquo", "not", "shy", "reg", "macr", "deg", "plusmn", "sup2", "sup3", "acute", "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14", "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde", "Auml", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc", "Euml", "Igrave", "Iacute", "Icirc", "Iuml", "ETH", "Ntilde", "Ograve", "Oacute", "Ocirc", "Otilde", "Ouml", "times", "Oslash", "Ugrave", "Uacute", "Ucirc", "Uuml", "Yacute", "THORN", "szlig", "agrave", "aacute", "acirc", "atilde", "auml", "aring", "aelig", "ccedil", "egrave", "eacute", "ecirc", "euml", "igrave", "iacute", "icirc", "iuml", "eth", "ntilde", "ograve", "oacute", "ocirc", "otilde", "ouml", "divide", "oslash", "ugrave", "uacute", "ucirc", "uuml", "yacute", "thorn", "yuml", "OElig", "oelig", "Scaron", "scaron", "Yuml", "fnof", "circ", "tilde", "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv", "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash", "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo", "dagger", "Dagger", "bull", "hellip", "permil", "prime", "Prime", "lsaquo", "rsaquo", "oline", "frasl", "euro", "image", "weierp", "real", "trade", "alefsym", "larr", "uarr", "rarr", "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr", "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni", "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang", "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp", "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe", "oplus", "otimes", "perp", "sdot", "vellip", "lceil", "rceil", "lfloor", "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts")
)
