\name{climate}
\alias{climate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns temperature, dew temperature, pressure, wind bearing and wind speed, of a given location.
}
\description{
Using the package "darksky" it's possible to get those weather reports.
}
\usage{
climate(Ta, La, date)
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{c2}{It's a vector containing:}
\item{c2[1]}{Temperature in Celsius.}
\item{c2[2]}{Dew temperature in Celsius.}
\item{c2[3]}{Air pressure in hPa.}
\item{c2[4]}{Wind bearing on 0º-360º scale. 0º means a wind blowing from North to South, 90º means East to West, 180º means South to North and, finally, 270º is West to East.}
\item{c2[5]}{Wind speed in m/s.}
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
function (Ta, La, date)
{
    require(darksky)
    require(weathermetrics)
    c1 = darksky::get_forecast_for(Ta, La, date)
    c2 = 0
    c2[1] = weathermetrics::fahrenheit.to.celsius(c1$currently$temperature,
        2)
    c2[2] = weathermetrics::fahrenheit.to.celsius(c1$currently$dewPoint,
        2)
    c2[3] = c1$currently$pressure
    c2[4] = c1$currently$windBearing
    c2[5] = c1$currently$windSpeed * (16.0934/36)
    return(c2)
  }

##latitude (T) and longitude (L) for São Francisco, Niterói, Brazil
##cycling circuit:
Ta=-22.915396
La=-43.095511
date="2018-04-12T19:00:00"
c=climate(Ta,La,date)
c
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
