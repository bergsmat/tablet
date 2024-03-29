% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablette.R
\name{tablet.tablette}
\alias{tablet.tablette}
\title{Convert to tablette from tablet.}
\usage{
\method{tablet}{tablette}(x, ...)
}
\arguments{
\item{x}{object of dispatch}

\item{...}{passed arguments}
}
\value{
tablette : a data.frame
 with columns for each combination of groups, and:
\item{_tablet_name}{observation identifier: character, possibly 'latex'; 
has a codelist attribute the values of which are the original column names}
\item{_tablet_level}{factor level, or the LHS of formulas in 'num'}
\item{All (or value of 'all' argument)}{ungrouped results}
}
\description{
Converts to 'tablette' from 'tablet'.
I.e., makes compact data.frame that emulates
the layout expected by \code{\link{as_kable}}.
Preserves attributes 'n', 'label', 'codelist', and 'latex' class if present.
}
\examples{
library(boot)
library(dplyr)
library(magrittr)
library(yamlet)
rm(melanoma)
melanoma \%<>\% select(-time, -year) 
melanoma \%<>\% decorate('
time:      [ Survival Time Since Operation, day               ]
status:    [ End of Study Patient Status,
           [ Alive: 2, Melanoma Death: 1, Unrelated Death: 3 ]]
sex:       [ Sex, [ Male: 1, Female: 0                       ]]
age:       [ Age at Time of Operation, year                   ]
thickness: [ Tumor Thickness, mm                              ]
ulcer:     [ Ulceration, [ Absent: 0, Present: 1             ]]
')
melanoma \%<>\% resolve
melanoma \%<>\% group_by(status, ulcer) 
melanoma \%<>\% tablet 
melanoma \%>\% header_rows
melanoma \%>\% as_kable
melanoma \%>\% tablette
melanoma \%>\% tablette \%>\% tablet
identical(melanoma, tablet(tablette(melanoma)))
  
}
\seealso{
Other tablet: 
\code{\link{as_tablet.data.frame}()},
\code{\link{as_tablet}()},
\code{\link{header_rows.tablet}()},
\code{\link{header_rows}()},
\code{\link{tablette.tablet}()}
}
\concept{tablet}
\keyword{internal}
