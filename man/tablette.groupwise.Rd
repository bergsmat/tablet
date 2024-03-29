% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablet.R
\name{tablette.groupwise}
\alias{tablette.groupwise}
\title{Generate a Group-wise Tablette}
\usage{
\method{tablette}{groupwise}(x, ..., all = "All", lab = list(lab ~ name + "\\n(N = " + n + ")"))
}
\arguments{
\item{x}{groupwise}

\item{...}{ignored}

\item{all}{a column name for ungrouped statistics; can have length zero to suppress ungrouped column}

\item{lab}{a list of formulas to generate column labels; \\n is translated as <br> in html context}
}
\value{
'tablette', with columns for each combination of groups, and:
\item{_tablet_name}{observation identifier}
\item{_tablet_level}{factor level (or special value 'numeric' for numerics)}
\item{_tablet_stat}{the LHS of formulas in 'fac' and 'num'}
\item{All (or value of 'all' argument)}{ungrouped results}
}
\description{
Generates a group-wise tablette. Calculates statistics
for all factors and numerics, with and without grouping
variables.  Column names represent finest level of
grouping, distinguished by attribute 'nest' (the values of
higher groups). Column names include 'all' column
for same statistics without groups. Result columns
have corresponding attribute 'n'. 'lab' supplies a label attribute
for each column where the RHS succeeds: by default appending 'n' to result column names.
}
\keyword{internal}
