\name{translate}
\alias{translate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Translate text with the Google or the Microsoft translation APIs.}
\description{This function provides easy access to the Google and Microsoft 
  Translation APIs via R. It can translate any language supported by
  the APIs (to see a list of the available languages, see the
  getGoogleLanguages() and getMicrosoftLanguages()
  functions). Text can be provided as either a column in a dataframe or
  as a single vector of text, where the elements are the documents to be
  translated. Translated text is returned in the format it was
  provided. If text is provided as a single vector, translate() returns
  a single vector of translated text. If a dataframe is provided, the
  user must specify which columns contains the text that is to be
  translated. Translated text is then bound to the dataframe and the
  entire dataframe is returned. The user must provide either a dataset
  and the content.field (column name) that contains the text to be
  translated, or a contect.vec (a character vector) where the elements
  are the text to be translated. Chris when it is appended to the data frame what is the new column called? Should there be a ne "new nae" option in this case?}
\usage{
translate(dataset = NULL, content.field = NULL, content.vec = NULL, api.key = NULL, client.id = NULL, client.secret = NULL, translator = "Google", source.lang = NULL, target.lang = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
A dataframe with a column containing the text to be translated. 
}
  \item{content.field}{
If a dataframe is passed to "dataset", the name of the column containing
the text must be passed to "content.field".
}
  \item{content.vec}{
A character vector of text. This is an alternative to "dataset"/"content.field".
}
  \item{api.key}{
Chris maybe this should be called google.api.key? To use the Google API, an API key must be provided. For more
information on getting your key, see https://developers.google.com/translate/v2/getting_started.
}
  \item{client.id}{
Chris maybe call this microsoft.client.id To use the Microsoft API, a client id and a client secret must be
provided. For more information on getting these, see
http://msdn.microsoft.com/en-us/library/hh454950.aspx. Chris tell people what a client secret is in one sentence...it sounds weird.
NOTE: you do not need to obtain an access token. translateR will
retrieve a token internally.
  }
  \item{client.secret}{
To use the Microsoft API, a client id and a client secret must be
provided. For more information on getting these, see
http://msdn.microsoft.com/en-us/library/hh454950.aspx.
NOTE: you do not need to obtain an access token. translateR will
retrieve a token internally.
}
  \item{translator}{
    The API that is to be used, either "Google" or "Microsoft".
    Google is the default translator but will not work without a
    user-supplied API key.
  }
  \item{source.lang}{
    The language code that corresponds with the language in which the
    source text is written. The translators use different language
    codes, so select accordingly. To see a list of language codes, enter
    getGoogleLanguages() or getMicrosoftLanguages() for Google or
    Microsoft, respectively.
  }
  \item{target.lang}{
    The language code that corresponds with the language into which the
    source text is to be translated. The translators use different language
    codes, so select accordingly. To see a list of language codes, enter
    getGoogleLanguages() or getMicrosoftLanguages() for Google or
    Microsoft, respectively.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

Chris make sure to define here what gets returned if appropriate?
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
