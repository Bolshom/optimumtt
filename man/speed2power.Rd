\name{speed2power}
\alias{speed2power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
For a given speed, the power necessary by a cyclist.
}
\description{
Using a given climate, bike specifications, athlete characteristics and terrain information, this function returns the power necessary to a cyclist being able to develop the input speed.
}
\usage{
speed2power(G, W, Crr, CdA, Rho, Ldt, Vw, phi, V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{G}{
Gradient of slope. It's given by the heigth of a climb divided by its distance.
}
  \item{W}{
Weigth of the system, athlete, bike, clothes and all the gearing necessary.
}
  \item{Crr}{
Coefficient of rolling resistance. The resistance of the tyres to the ground.
}
  \item{CdA}{
It's the drag coefficient of the system. Usually the range is .4 (tops) to .17 (Obree 1994)
}
  \item{Rho}{
The air density in kg/m^3.
}
  \item{Ldt}{
The percentual loss of the drivetrain. Usually ranges from 1.5~5 percent.
}
  \item{Vw}{
Wind speed in m/s.
}
  \item{phi}{
The angle at which the wind affects the cyclist.
}
  \item{V}{
The speed in km/h.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{power}{The power, in watts, necessary to overcome the resistance, by a cyclist.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://www.gribble.org/cycling/power_v_speed.html
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
function (G, W, Crr, CdA, Rho, Ldt, Vw, phi, V)
{
    power = (((1 - (Ldt/100))^(-1)) * f.resist(G, W, Crr, CdA,
        Rho, V, Vw, phi) * (V/3.6))
    return(power)
  }

w=65+11
cda=.3
dt=3
g=0
crr=.005
rho=1.226
vw=0
phi=0
v=30

power=speed2power(g,w,crr,cda,rho,dt,vw,phi,v)
power
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
