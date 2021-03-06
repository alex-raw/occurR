% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assoc.R
\name{v_assoc}
\alias{v_assoc}
\title{Vectorized calculation of association measures}
\usage{
v_assoc(f1, o11, f2 = NULL, n = NULL, fun = "ll")
}
\arguments{
\item{f1}{numeric}

\item{o11}{numeric joint frequencies}

\item{f2}{numeric, if not provided, sum of o11 is used}

\item{n}{numeric, if not provided, sum of f1 is used}

\item{fun}{character vector for built-in measures (see Details).
alternatively a custom function, expression, or call. if names are supplied,
they are used in the output}
}
\value{
matrix
}
\description{
Calculates association measures from vectors of frequencies and
joint frequencies.
}
\details{
coming soon ...
}
