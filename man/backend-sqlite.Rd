% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backend-sqlite.R
\name{backend-sqlite}
\alias{simulate_sqlite}
\title{Backend: SQLite}
\usage{
simulate_sqlite()
}
\description{
See \code{vignette("translation-function")} and \code{vignette("translation-verb")} for
details of overall translation technology. Key differences for this backend
are:
\itemize{
\item Uses non-standard \code{LOG()} function
\item Date-time extraction functions from lubridate
\item Custom median translation
\item Right and full joins are simulated using left joins
}

Use \code{simulate_sqlite()} with \code{lazy_frame()} to see simulated SQL without
converting to live access database.
}
\examples{
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_sqlite())
lf \%>\% transmute(x = paste(c, " times"))
lf \%>\% transmute(x = log(b), y = log(b, base = 2))
}
