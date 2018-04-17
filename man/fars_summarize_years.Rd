\name{fars_summarize_years}
\alias{fars_summarize_years}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the aggregation of record q-ty per annum
}
\description{
The function aggregates the quantity of records per year.
}
\usage{
fars_summarize_years(years)
}

\arguments{
  \item{years}{
he set of years (integer or character vector of the proper format) we are expecting to read data. IF the set of years containg the wrong year so the function execution will be stopped only for this year and the error message will be compiled for this year.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns the dataframe of year and quantity.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# years_output <- fars_summarize_years(c(1978, 1979))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
