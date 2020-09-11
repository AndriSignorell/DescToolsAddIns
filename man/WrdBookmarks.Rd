\name{Operating MS Word}
\alias{ToWrd}
\alias{ToWrdWithBookmark}
\alias{UpdateBookmark}
\alias{CreateBookmark}
\alias{SelectBookmark}
\alias{DeleteBookmark}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Operating MS Word
%%  ~~function to do ... ~~
}
\description{
Controlling MS Word with RStudio has many convenient advantages. We can quickly create some reports on the fly and also use bookmarks to create dynamically updatable report chunks such as tables or graphics. Here we implement a set of functions to handle bookmarks in MS Word.
}

\usage{
ToWrd()
ToWrdWithBookmark()

CreateBookmark()
SelectBookmark()
UpdateBookmark()
DeleteBookmark()
}
\details{
\code{ToWrd()} simply sends the selected object to the current Word instance. The Word summary logic is generic and depends on the object. There are summaries for characters, tables, models etc.. The inserted text is however not later updateable; as soon as it is inserted in the Word document the connection is lost.

\code{ToWrdWithBookmark()} inserts the object in the Word document and defines a bookmark that covers the whole inserted object (including tables or graphics). Then the selected command chunk in the Rstudio source pane is enclosed in brackets and the name of the new defined bookmark is inserted as meta tag. If the R-code is later changed, the output in Word can simply be refreshed by selecting the code chunk in RStudio (including the meta tag) and click on \code{UpdateBookmark()}.

\code{CreateBookmark()} would create a bookmark on the currently selected text in Word and use the currently selected text in RStudio as name. \code{SelectBookmark()}, \code{DeleteBookmark()} behave analogously.

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\keyword{ utilities }
