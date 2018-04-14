\name{Phi}
\alias{Phi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the angle, in degrees, that the wind affects the cyclist.
}
\description{
By evaluating the direction that a certain cyclist is going, and getting the wind bearing, it's possible to calculate this angle phi.
}
\usage{
Phi(Ta, La, Tb, Lb, date)
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
  \item{date}{
Date and time to get the weather report. Check example for input format.
}
}
\details{
To use the package "darksky" to collect weather report, you must activate the API with your key.
Check https://darksky.net/dev for more info.
}
\value{
\item{phi}{The angle at which the wind affects the cyclist, in degrees.}
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
Natan Freitas Leite
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
function (Ta, La, Tb, Lb, date)
{
    phi = abs(ang_head(Ta, La, Tb, Lb) - climate(Ta, La, date)[4])
    return(phi)
  }

##latitude (T) and longitude (L) for São Francisco, Niterói, Brazil
##cycling circuit:
Ta=-22.915396
La=-43.095511
Tb=-22.934862
Lb=-43.101902
##date to get the wind direction in relation to the cyclist
##"AAAA-MM-DDThh:mm:ss"
date="2017-04-02T07:00:00"
##result
phi=Phi(Ta,La,Tb,Lb,date)
phi
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
