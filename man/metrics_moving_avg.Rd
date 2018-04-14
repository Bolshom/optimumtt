\name{metrics_moving_avg}
\alias{metrics_moving_avg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives a PMax, Wbal and mFTP estimations from a given period of training.
}
\description{
From a mean maximum power file, containing the best power marks during a certain period for every training sessions, this function is able to calculate the Pmax, WBal and mFTP estimations for a season.
}
\usage{
metrics_moving_avg(updt, mmp_alldata, dates_list, period = 90, wpk = F, weight, pmax_all = NULL, wbal_all = NULL, mftp_all = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{updt}{
A boolean indicating wheter it's to update with newer training sessions (updt=T), or to create from scratch (updt=F).
}
  \item{mmp_alldata}{
The mean maximum power from each training session. It's the result of mmp_alldata function inside this package.
}
  \item{period}{
The period of days to calculate the moving average. The default is 90 days.
}
  \item{wpk}{
A boolean to indicate wheter the power information are in watts (wpk=F) or watts per kilo (wpk=T).
}
  \item{weight}{
The weight of the athlete in kilos.
}
  \item{dates_list}{
A vector containing all dates and times from every training session. It's good to use with get_power_data() within Golden Cheetah.
}
  \item{pmax_all}{
If updt=T, then this is the older vector of PMax estimations.
}
  \item{wbal_all}{
If updt=T, then this is the older vector of WBal estimations.
}
  \item{mftp_all}{
If updt=T, then this is the older vector of mFTP estimations.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{pmax}{The maximum power estimation for each 90-day period, in watts.}
\item{wbal}{The anaerobic capacity estimations for each 90-day period, in Joules.}
\item{mftp}{The modeled functional threshold power estimation for each 90-day period, in watts.}
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
function (updt, mmp_alldata, dates_list, period = 90, wpk = F,
    weight, pmax_all = NULL, wbal_all = NULL, mftp_all = NULL)
{
    require(zoo)
    require(changepoint)
    require(lubridate)
    options(warn = -1)
    len = c()
    l_ma = length(mmp_alldata)
    for (i in 1:l_ma) {
        len[i] = length(mmp_alldata[[i]])
    }
    mmp_all_df = data.frame(1:max(len))
    for (i in 1:l_ma) {
        if (length(mmp_alldata[[i]] < max(len))) {
            mmp_all_df[, i] = c(mmp_alldata[[i]], rep(0, max(len) -
                length(mmp_alldata[[i]])))
        }
        else {
            mmp_all_df[, i] = mmp_alldata[[i]]
        }
    }
    if (!wpk) {
        mmp_all_df = mmp_all_df/weight
    }
    dates_list_2 = lubridate::date(dates_list)
    dates = dates_list_2[1]:tail(dates_list_2, 1)
    dates = as.Date(dates, origin = "1970-01-01")
    if (!updt) {
        pmax_all = c()
        mftp_all = c()
        wbal_all = c()
        condition = as.numeric(tail(dates_list_2, 1) - dates_list_2[1]) -
            period + 1
        for (i in 1:condition) {
            if (length(which(dates_list_2 \%in\% (dates[i] - period):dates[i])) ==
                1) {
                max_mmp = mmp_all_df[, which(dates_list_2 \%in\%
                  (dates[i] - period):dates[i])]
                metrics = metrics_model(max_mmp, T, weight)
                pmax_all[i] = metrics[1]
                mftp_all[i] = metrics[2]
                wbal_all[i] = metrics[3]
                print(i)
            }
            else {
                max_mmp = apply(mmp_all_df[, which(dates_list_2 \%in\%
                  dates[i]:(dates[i] + period))], 1, max)
                metrics = metrics_model(max_mmp, T, weight)
                pmax_all[i] = metrics[1]
                mftp_all[i] = metrics[2]
                wbal_all[i] = metrics[3]
                print(i)
            }
        }
    }
    else {
        condition1 = length(pmax_all) + 1
        condition2 = as.numeric(tail(dates_list_2, 1) - dates_list_2[1]) -
            period + 1
        for (i in condition1:condition2) {
            max_mmp = apply(mmp_all_df[, which(dates_list_2 \%in\%
                dates[i]:(dates[i] + period))], 1, max)
            metrics = metrics_model(max_mmp, T, weight)
            pmax_all[i] = metrics[1]
            mftp_all[i] = metrics[2]
            wbal_all[i] = metrics[3]
            print(i)
        }
    }
    options(warn = 0)
    return(list(pmax_all, mftp_all, wbal_all))
  }

  data(mmp_alldata_dataset)
  data(dates_list_updt)

  metrics=metrics_moving_avg(F,mmp_alldata_dataset,dates_list_updt,weight=65)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
