\name{mmp_alldata}
\alias{mmp_alldata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the mean maximum power for a set of training sessions.
}
\description{
Returns the moving maximum power for a bunch of activities, using the power data second-by-second, from all durations to the length of the training sessions.
The input must be in form of a list.
}
\usage{
mmp_alldata(act_power_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{act_power_list}{
A list containing the vector power data from a set of training sessions. It's good to use with get_power_data() within Golden Cheetah.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It gives the mean maximum power for each activity within the power list.
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
function (act_power_list)
{
    mmp_alldata = list()
    l_pl = length(act_power_list)
    for (i in 1:l_pl) {
        mmp_alldata[[i]] = mmp_act(act_power_list[[i]])
        print(i)
    }
    return(mmp_alldata)
  }

  data(power_list)

  mmp_sessions=mmp_alldata(power_list[1:10]) #bigger files will take longer to run.
  plot(mmp_sessions[[1]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
