\name{ang_head}
\alias{ang_head}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The angle from point A to B.
}
\description{
Giver the angle from a origin to a certain destination.
}
\usage{
ang_head(Ta, La, Tb, Lb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ta}{
Latitude of the origin point.
}
  \item{La}{
Longitude of the origin point.
}
  \item{Tb}{
Latitude of the destination point.
}
  \item{Lb}{
Longitude of the destination point.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{b}{The angle direction.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/
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
function (Ta, La, Tb, Lb)
{
    Ta = rad(Ta)
    Tb = rad(Tb)
    La = rad(La)
    Lb = rad(Lb)
    dL = abs(Lb - La)
    x = cos(Tb) * sin(dL)
    y = cos(Ta) * sin(Tb) - sin(Ta) * cos(Tb) * cos(dL)
    b = atan2(x, y)
    b = deg(b)
    return(b)
  }

##Kansas City: 39.099912, -94.581213 -> St Louis: 38.627089, -90.200203
##Result should be: ang.head=96.5126242349993
a=ang_head(39.099912, -94.581213,38.627089, -90.200203)
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
