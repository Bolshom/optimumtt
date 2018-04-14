\name{Rho_loc}
\alias{Rho_loc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives the air density for a certain location.
}
\description{
Returns air density, of a given location, by using the package "darksky."
}
\usage{
Rho_loc(Ta, La, date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ta}{
Latitude of a given location.
}
  \item{La}{
Longitude of a given location.
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
\item{b}{The air density in kg/m^3 of a given location.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://www.gribble.org/cycling/air_density.html
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
function (Ta, La, date)
{
    clim = climate(Ta, La, date)
    t = clim[1]
    t.dew = clim[2]
    hpa = clim[3]
    b = ((P.d(hpa, t.dew)/(287.0531 * (t + 273.15))) + (P.v(t.dew)/(461.4964 *
        (t + 273.15))))
    return(b)
  }

##latitude (T) and longitude (L) for São Francisco, Niterói, Brazil
##cycling circuit:
Ta=-22.915396
La=-43.095511
date="2018-04-12T19:00:00"
rho=Rho_loc(Ta,La,date)
rho
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
