\name{fars_map_state}
\alias{fars_map_state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build the map chart to visualize the annual results per a state.~
}
\description{
The function is reading raw data, manipulating them and develop the visualization via a map chart.
}
\usage{
fars_map_state(state.num, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state.num}{
The number of the state to read archived data (integer of character).  It could be as integer or character vector. If state.num variable is of the wrong format or value so the function is stopped and error message is generated.
}
  \item{year}{
The year of the accident data to be read from archive file. It could be as integer or character vector. If year variable is of the wrong format so the function is stopped and R internal message is generated.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the graphic object (image).
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
\code{\link{fars_read}}, \code{\link{make_filename}}, \code{\link{fars_read_years}}, \code{\link{fars_summarize_years}}
}
\examples{
## years_output <- fars_map_state(num.state, year)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
