% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{check_frequency_list}
\alias{check_frequency_list}
\title{Check frequency lists for common problems}
\usage{
check_frequency_list(.x, case_sensitive = FALSE)
}
\arguments{
\item{.x}{frequency list with a character/factor column with types and a
numeric column with counts}

\item{case_sensitive}{logical whether or not the frequency list is case
sensitive}
}
\value{
NULL; fails if expected columns are ambiguous or missing, warns if
duplicate types or empty types are found
}
\description{
Check frequency lists for duplicates caused by whitespace or case
sensitivity. Also warn if any fields are empty. This is a convenience
function. Do not rely on this feature during your data cleaup.
More checks might be added in future versions.
}
\examples{
# TODO

}
