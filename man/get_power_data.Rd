\name{get_power_data}
\alias{get_power_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns power data from every training within a period. GOLDEN CHEETAH ONLY!
}
\description{
A function to get all power data from every training session throughout a selected period. It only works on Golden Cheetah.
}
\usage{
get_power_data()
}
%- maybe also 'usage' for other objects documented here.
\details{
It will run on selection under the tab "Trends".
}
\value{
\item{dates_list_updt}{A vector containing all dates and times from every training session.}
\item{power_list_updt}{A list containing power data inside of vectors, from all activities on a selected period.}
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
function ()
{
    metrics = GC.season.metrics()
    metrics = subset(metrics, !Average_Power == 0)
    rownames(metrics) = 1:dim(metrics)[1]
    dates_list_updt = metrics$time
    power_list_updt = list()
    for (i in 1:length(dates_list_updt)) {
        power_list_updt[[i]] = GC.activity(activity = dates_list_updt[i])[[1]]$power
    }
    return(list(dates_list_updt, power_list_updt))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
