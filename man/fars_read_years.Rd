\name{fars_read_years}
\alias{fars_read_years}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read data for sevelar years
}
\description{
The function is designed to read data for the set of years.
}
\usage{
fars_read_years(years)
}

\arguments{
  \item{years}{
the set of years (integer or character vector of the proper format) we are expecting to read data.IF the set of years containg the wrong year so the function execution will be stopped only for this year and the error message will be compiled for this year.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns the output of lapply funtion (it is a list). Each item of the list is the separate year dataset.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Demyd Dzyuban
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fars_read}}, \code{\link{make_filename}}
}
\examples{
years_output <- fars_read_years(c(1978, 197))
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
