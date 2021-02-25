\name{Operating MS Word}
\alias{ToWrd}
\alias{ToWrdWithBookmark}
\alias{UpdateBookmark}
\alias{CreateBookmark}
\alias{SelectBookmark}
\alias{DeleteBookmark}
\alias{RecreateBookmarkChunk}
\alias{WrdBookmarks}
\alias{RenameBookmark}

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

WrdBookmarks()
CreateBookmark()
SelectBookmark()
UpdateBookmark()
DeleteBookmark()

RenameBookmark(name, newname, wrd = DescToolsOptions("lastWord"))

RecreateBookmarkChunk()

}

\arguments{
  \item{name}{the name of the bookmark.}
  \item{newname}{the new name of the bookmark.}
  
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}

\details{
\code{WrdBookmarks()} returns a data frame with all bookmarks in the current document (given by a handle created with \code{GetCurrWrd()} or \code{GetNewWrd()} and stored in \code{DescToolsOptions("lastWord"))}, containing the bookmark's name, its id and type \code{("text", "plot", "other")}, as well as the page number in the document where it starts.
\preformatted{
   > DescToolsAddIns::WrdBookmarks()
     id         name pagenr type
   1  2 bmt182295322      1 text
   2  3 bmp937470551      1 plot
}

\code{ToWrd()} directly sends the evaluation of the selected object in the source panel to the current Word instance. The function is generic and acts in dependency on the selected object. So far there are interfaces for characters, tables, models. Note that the inserted text is no longer updateable; as soon as it is inserted in the Word document the connection to it is lost.

\code{ToWrdWithBookmark()} inserts the object in the Word document and defines a bookmark that covers the whole range of the inserted object (including tables or graphics). Then the selected command chunk in the Rstudio source pane is enclosed in brackets and the name of the new defined bookmark is therein inserted as meta tag. If the R-code is later changed, the output in Word can simply be refreshed by selecting the code chunk in RStudio (including the meta tag) and click on the menu \code{UpdateBookmark()} (of course it's convenient to have a shortcut defined for that).

\code{CreateBookmark()} creates a bookmark at the current cursor position in a in an open Word  document,  using the currently selected text in RStudio as name. \code{SelectBookmark()}, \code{DeleteBookmark()} behave analogously.

\code{RecreateBookmarkChunk()} recreates the selected chunks in RStudio by inserting a bookmark if not existing and updating it with the result. 

With the \code{BookmarkDlg()} there's an interactive support for handling bookmarks. Found bookmarks are presented in a sortable and filterable listbos. Selecting, deleting and renaming the selected bookmark is supported. 

\figure{BookmarkDlg.png}{ Bookmark dialog}

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\keyword{ utilities }
