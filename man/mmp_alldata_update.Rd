\name{mmp_alldata_update}
\alias{mmp_alldata_update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the mean maximum power updated for a set of training sessions.
}
\description{
Returns the moving maximum power for a bunch of activities, using the power data second-by-second, from all durations to the length of the training sessions.
It adds mean maximum power information from new training sessions, based on an older one.
}
\usage{
mmp_alldata_update(dates_list_updt, dates_list, power_list_updt, mmp_alldata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates_list_updt}{
The dates and time vector from a period of training.
}
  \item{dates_list}{
The older dates and time vector from a period of training.
}
  \item{power_list_updt}{
A list containing the vector power data from a set of training sessions.
}
  \item{mmp_alldata}{
The older mean maximum power data calculated before.
}
}
\details{
It's good to use with get_power_data() within Golden Cheetah.
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
function (dates_list_updt, dates_list, power_list_updt, mmp_alldata)
{
    updt = length(which(!(dates_list_updt \%in\% dates_list))) ==
        0
    if (updt) {
        mmp_alldata_updt = mmp_alldata
    }
    else {
        mmp_updt = list()
        condition = which(!(dates_list_updt \%in\% dates_list))
        for (i in condition) {
            mmp_updt[[i]] = mmp_act(power_list_updt[[i]])
        }
        mmp_alldata_updt = list()
        l_lu = length(list_updt)
        condition_2 = which((list_updt \%in\% list))
        for (i in 1:l_lu) {
            if (i \%in\% condition) {
                mmp_alldata_updt[[i]] = mmp_alldata[[which(list ==
                  list_updt[i])]]
            }
            else {
                mmp_alldata_updt[[i]] = mmp_updt[[i]]
            }
        }
    }
    return(mmp_alldata_updt)
  }

  data(dates_list_updt)
  data(power_list)
  data(mmp_alldata)

  dates_list=dates_list_updt[-431]
  mmp_alldata=mmp_alldata[1:430]

  mmp_sessions=mmp_alldata_update(dates_list_updt,dates_list,power_list_updt,mmp_alldata)
  plot(mmp_sessions[[1]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
