\name{Rho}
\alias{Rho}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives the air density for a certain atmosphere condition.
}
\description{
By inputting temperature, dew temperature and air pressure, it's possible to calculate the air density in kg/m^3.
}
\usage{
Rho(t, t.dew, hpa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
Temperature in Celsius.
}
  \item{t.dew}{
Dew temperature in Celsius.
}
  \item{hpa}{
Air pressure in hPa.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{rho}{Returns the air density calculation in kg/m^3.}
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
function (t, t.dew, hpa)
{
    rho = ((P.d(hpa, t.dew)/(287.0531 * (t + 273.15))) + (P.v(t.dew)/(461.4964 *
        (t + 273.15))))
    return(rho)
  }

t=23.2 #celsius
t.dew=20 #celsius
hpa=1012.1 #hpa
rho=Rho(t,t.dew,hpa)
rho
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
