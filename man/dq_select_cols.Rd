% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_dq.R
\name{dq_select_cols}
\alias{dq_select_cols}
\title{Select default display columns for Data Quality Tables}
\usage{
dq_select_cols(
  x,
  ...,
  default = list("UniqueID", "EnrollmentID", `Entry Date` = "EntryDate", "Type", "Issue")
)
}
\arguments{
\item{x}{\code{(data.frame)}}

\item{...}{\code{(columns to select)} These can be unquoted or quoted.}

\item{default}{\code{(list)} Columns to select can also be supplied as a list. If using `...` and you wish to not select the default columns, set to `FALSE`}
}
\value{
\code{(data.frame)} with selected columns. See `default` argument for defaults that will be selected.
}
\description{
Select default display columns for Data Quality Tables
}
\examples{
dq_select_cols(data.frame(UniqueID = 1:3, Issue = letters[1:3], EnrollmentID = 2:4, Type = letters[4:6],
EntryDate = 1:3, blah = 1:3), blah)
}
