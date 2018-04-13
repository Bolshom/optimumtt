\name{rad}
\alias{rad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts degrees to radians.
}
\description{
Converts degrees to radians.
}
\usage{
rad(deg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deg}{
Angle in degrees.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Angle in radians.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Natan Freitas Leite.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
function (deg)
{
    (deg * pi)/(180)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
