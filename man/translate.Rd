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
  user must specify which column contains the text that is to be
  translated. Translated text is then bound to the dataframe in a new
  column named "translatedContent" and the
  entire dataframe is returned. The user must provide either a dataset
  and the content.field (column name) that contains the text to be
  translated, or a contect.vec (a character vector) where the elements
  are the text to be translated.}
\usage{
translate(dataset = NULL, content.field = NULL, content.vec = NULL, google.api.key = NULL, microsoft.client.id = NULL, microsoft.client.secret = NULL, source.lang = NULL, target.lang = NULL)
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
  \item{google.api.key}{
To use the Google API, an API key must be provided. For more
information on getting your key, see http://support.google.com/cloud/answer/6158862?hl=en .
}
\item{microsoft.api.key}{
To use the Microsoft API, an API key must be provided from Microsoft Azure Cognitive Services. For
more information on getting your key, see http://www.microsoft.com/en-us/translator/trial.aspx .
}
\item{microsoft.token}{
While using the Microsoft API, you have choice of using access token or not. If value is set as TRUE, then
translateR will retrieve a token internally and process the request. Token method is recommended for longer translation.
Default value is set as FALSE. 
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
\code{translate} returns a dataframe if it is passed a dataframe and a
character vector if it is passed a character vector.
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
\dontrun{
# Load example data. Three columns, the text content ('email') and two metadata fields (date and subject)
data(enron)

# Google, translate column in dataset
google.dataset.out <- translate(dataset = enron, content.field = 'email', google.api.key = my.api.key,
                         source.lang = 'en', target.lang = 'de')

# Google, translate vector
google.vector.out <- translate(content.vec = enron$email, google.api.key = my.api.key,
                         source.lang = 'en', target.lang = 'de')

# Microsoft, translate column in dataset
google.dataset.out <- translate(dataset = enron, content.field = 'email', microsoft.client.id = my.client.id,
                         microsoft.client.secret = my.client.secret, source.lang = 'en', target.lang = 'de')

# Microsoft, translate vector
google.vector.out <- translate(content.vec = enron$email, microsoft.client.id = my.client.id,
                        microsoft.client.secret = my.client.secret, source.lang = 'en',
                        target.lang = 'de')
}
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
