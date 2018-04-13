\name{mmp_act}
\alias{mmp_act}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the mean maximum power for a given activity.
}
\description{
Returns the moving maximum power for a given activity, using the power data second-by-second, from all durations to the length of the training session.
}
\usage{
mmp_act(activity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{activity}{
A vector containing the second-by-second power data in watts, from an activity.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output of this function is a vector of all mean maximum power, from all durations.
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
mmp_act=function(activity){
  require(zoo)
  mmp_vec=c()
  l_act=length(activity)

  for (i in 1:l_act){
    mmp_vec[i]=max(rollmean(activity,i))

  }

  return(mmp_vec)

}

data(power_list)

mmp=mmp_act(power_list[[1]])
plot(mmp)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
