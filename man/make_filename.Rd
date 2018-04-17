\name{make_filename}
\alias{make_filename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make the file name of the predefined template
}
\description{
The function is designed to compile the file name of the perdifined template. The only variable (input) parameter is YEAR
}
\usage{
make_filename(year)
}

\arguments{
  \item{year}{
the set of years (integer or character vector of the proper format) we are expecting to read data.IF the set of years containg the wrong year so the function execution will be stopped only for this year and the error message will be compiled for this year.
}
}
\details{
The function uses \code{sprintf()} to compile a string value.
}
\value{
The function return is a character vector containing the file name required.
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
# filename <- make_filename('1978')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
