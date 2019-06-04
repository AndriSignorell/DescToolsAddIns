\name{FileOpenDlg}
\alias{FileOpenDlg}
\title{Get the Path of a Data File to Be Opened
}
\description{Handling of pathnames is tedious in Windows because of the backslashes, that prevent simple pasting of a copied path into the source code. \code{FileOpenDlg()} displays the FileOpen-Dialog for picking a file interactively. When done, backslashes in the returned path will be replaced by simple slashes as used in R path definitions. The edited path is returned.}
\usage{
FileOpenDlg(fmt = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmt}{the format, in which the filename parts should be returned. This can be defined quite flexible containing the elements path, filename and ext (file extension) enclosed by \%.\cr
    Default is path/filename.ext and coded as "\%path\%\%filename\%.\%ext\%".
    See examples for additional alternative definitions.
}
}

\details{
    There are 2 predefined formats that can be addressed with \code{fmt = 1} resp. \code{fmt = 2}.
    1 will return the path enclosed by a typical \code{read.table(...)} command, 2 will do the same with \code{load(...)}.

}

\value{character string containing the path
  (also present in the clipboard)
  }
\author{Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{file.choose}}
}

\examples{
\dontrun{
# choose a file
fn <- FileOpenDlg()
print(gettextf("You chose the file: \%s ", fn))

# the path and filename can as well be nested in a command,
#   done here to build a read.table command that can be well inserted into the code:
FileOpenDlg(fmt="d.\%fname\% <- read.table(file = \"\%path\%\\\%fname\%.\%fxt\%\",
  header = TRUE, sep = \";\", na.strings = c(\"NA\",\"NULL\"), strip.white = TRUE)")

# go to your editor and paste...
}
}
\keyword{ manip }
