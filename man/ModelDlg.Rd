\name{ModelDlg}
\alias{ModelDlg}

\title{Compose Model Formula}

\description{\code{ModelDlg()} allows to compose model formulas based on the variablenames of a \code{data.frame}. The available variables can be filtered and sorted, which is helpful to keep the overview. Additional orientation is provided by the comments with which variables can be assigned. (See  \code{\link[DescTools]{Label}()}). The labels will be displayed below the variable list.

The model dialog can also be used to edit an already existing formula. Simply select the model formula (including the data argument) and start the dialog.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ModelDlg(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data frame containing the variables for the model.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{further arguments to be passed to the default function. }
}

\details{
In the model dialog, the variablenames (extended with their class) of the selected data.frame are listed on the right, from where they can be inserted in the model box by clicking on a button between the two boxes. Clicking on the \code{+} button will use + to concatenate the variablenames.
The same applies analogously to \code{*} and \code{:}. \eqn{x^2}{x2} inserts the chunk \code{poly(<varname>, 2)}, the grade of the polynom can easily be changed in the formula edit field afterwards. 

Any criteria for the variablename can be entered in the filterbox. The filter will be dynamically applied to the list. In order to get rid of the filter just empty the filterbox.

\figure{ModelDlg.png}{Model dialog}

After clicking on ok, the formula \code{temperature ~ area + driver + delivery_min, data=d.pizza} will be inserted on the cursor position.

An already constructed formula can also be provided to the function. The model dialog will separate the single parts and populate the according textboxes.
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
