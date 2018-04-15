\name{fars_read}
\alias{fars_read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read files
}
\description{
The function read data from files specified by the user
}
\usage{
fars_read(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
the file name of the data source is the input character vector. I could be defined by the user of other application. If the file does not exist the function is terminated and the proper message is generated.
}
}
\details{
The function uses readr::read_csv() to read input data
}
\value{
the function return the dataframe of 'tbl_df' class.

}
\references{
No references are assumed.
}
\author{
Demyd Dzyuban
}
\note{
%%  ~~further notes~~
}

\section{Warning }{
Stop reading if the file does not exist.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
input_data <- fars_read("data_source.txt")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
