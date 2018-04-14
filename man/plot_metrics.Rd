\name{plot_metrics}
\alias{plot_metrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of cycling metrics.
}
\description{
It generates the plot for the 3 metrics calculated using the function metrics_moving_avg.
}
\usage{
plot_metrics(dates_list, pmax_all = NULL, wbal_all = NULL, mftp_all = NULL, period = 90)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates_list}{
A vector containing all dates and times from every training session. It's good to use with get_power_data() within Golden Cheetah.
}
  \item{pmax_all}{
The maximum power estimation for each 90-day period, in watts. It's the output of metrics_moving_avg.
}
  \item{wbal_all}{
The anaerobic capacity estimations for each 90-day period, in Joules. It's the output of metrics_moving_avg.
}
  \item{mftp_all}{
The modeled functional threshold power estimation for each 90-day period, in watts. It's the output of metrics_moving_avg.
}
  \item{period}{
The period of days to calculate the moving average. The default is 90 days as in function metrics_moving_avg.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It prints 3 plots about the metrics PMax, WBal and mFTP, for each date between the first and last dates in dates_list.
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
function (dates_list, pmax_all = NULL, wbal_all = NULL, mftp_all = NULL,
    period = 90)
{
    if (length(pmax_all) != 0) {
        plot(seq(dates_list[1] + period, tail(dates_list, 1),
            length.out = length(pmax_all)), pmax_all, type = "l",
            lwd = 2, col = "purple", xaxt = "n", main = paste("PMax: Rolling",
                period, "days"), xlab = "Date", ylab = "Watts")
        axis(side = 1, at = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), labels = format(seq(dates_list[1] +
            period, tail(dates_list, 1), length.out = 8), "\%b-\%y"))
        abline(v = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), col = "grey", lwd = 0.5, lty = "dotted")
        grid(NA, NULL)
    }
    if (length(wbal_all) != 0) {
        plot(seq(dates_list[1] + period, tail(dates_list, 1),
            length.out = length(wbal_all)), wbal_all, type = "l",
            lwd = 2, col = "orange", xaxt = "n", main = paste("W': Rolling",
                period, "days"), xlab = "Date", ylab = "Joules")
        axis(side = 1, at = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), labels = format(seq(dates_list[1] +
            period, tail(dates_list, 1), length.out = 8), "\%b-\%y"))
        abline(v = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), col = "grey", lwd = 0.5, lty = "dotted")
        grid(NA, NULL)
    }
    if (length(mftp_all) != 0) {
        plot(seq(dates_list[1] + period, tail(dates_list, 1),
            length.out = length(mftp_all)), mftp_all, type = "l",
            lwd = 2, col = "red", xaxt = "n", main = paste("Modeled FTP: Rolling",
                period, "days"), xlab = "Date", ylab = "Watts")
        axis(side = 1, at = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), labels = format(seq(dates_list[1] +
            period, tail(dates_list, 1), length.out = 8), "\%b-\%y"))
        abline(v = seq(dates_list[1] + period, tail(dates_list,
            1), length.out = 8), col = "grey", lwd = 0.5, lty = "dotted")
        grid(NA, NULL)
    }
  }

  data(metrics)

  par(mfrow=c(1,3))
  plot_metrics(dates_list_updt,metrics[[1]],metrics[[2]],metrics[[3]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
