\name{ModelDlg}
\alias{ModelDlg}

\title{Compose Model Formula}

\description{\code{ModelDlg()} allows to compose model formulas based on the variablenames of a \code{data.frame}. The available variables form a data frame can be filtered and sorted, which is helpful to keep the overview.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ModelDlg(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object containing the elements to be selected. x can be a data.frame, a factor or any other vector.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{further arguments to be passed to the default function. }
}

\details{
In the model dialog, the variablenames of the selected data.frame are listed on the right, from where they can be inserted in the model box by clicking on a button between the two boxes. Clicking on the \code{+} button will use + to concatenate the variablenames.

\figure{ModelDlg.png}{Model dialog}

After clicking on ok, the formula \code{temperature ~ area + driver + delivery_min, data=d.pizza} will be inserted on the cursor position.
}
\value{
the model formula
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{formula}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
ModelDlg(d.pizza)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
